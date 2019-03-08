{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
-- | An implementation of the basic primitives of JSON-RPC 2.0.
module Argo.JSONRPC
  ( -- * Primary interface to JSON-RPC
    App
  , mkApp
  , Method(..)
  , Command
  , command
  , runCommand
  , Query
  , query
  , nullQuery
  , runQuery
  , Notification
  , notification
  , runNotification
  , stateless
  , IsMethod(..)
  , setState
  -- * JSON-RPC exceptions
  , JSONRPCException(..)
  , raise
  , parseError
  , methodNotFound
  , invalidRequest
  , invalidParams
  , internalError
  -- * Serving applications over transports
  , serveStdIO
  , serveHandles
  , serveStdIONS
  , serveHandlesNS
  -- *
  , RequestID(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception hiding (TypeError)
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.:!), (.=))
import Data.Coerce
import GHC.Exts (Constraint)
import GHC.TypeLits
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)
import Data.Binary.Builder
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as SBS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Network.Wai (strictRequestBody)
import System.IO
import Web.Scotty hiding (raise, params, get, put)

import Debug.Trace

import Argo.Netstring

-- | We only support JSON-RPC 2.0.
jsonRPCVersion :: Text
jsonRPCVersion = "2.0"

{-
A server can receive requests or notifications, and must reply to them.

A server has _state_, which is stored in an MVar to allow easy
concurrency, a collection of methods, each of which is a function
from the JSON value that is the parameter to a JSON value that is the
response.

Methods come in three forms:
 - commands, which can modify state and return an answer to the client;
 - queries, which return an answer but do not modify state; and
 - notifications, which can modify state but do not return an answer.
-}

-- TODO: use parametricity to prevent mucking around with RequestIDs

-- | A server is a mapping from method names to these methods.
data Method s where
  -- | A @Command@ may modify server state and returns a response
  Command      :: (JSON.Value -> Command      s JSON.Value) -> Method s
  -- | A @Query@ may not modify server state but does return a response
  Query        :: (JSON.Value -> Query        s JSON.Value) -> Method s
  -- | A @Notification@ may modify server state but does not return a response
  Notification :: (JSON.Value -> Notification s ())         -> Method s

-- | A Command can both modify the server's state and send a reply to the user.
newtype Command s a
  = C (ReaderT RequestID (StateT s IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

runCommand :: MonadIO m => Command s a -> RequestID -> s -> m (s, a)
runCommand c r s = liftIO $
  do (a, s') <- coerce c r s
     pure (s', a)

command ::
  forall params result s.
  (JSON.FromJSON params, JSON.ToJSON result) =>
  (params -> Command s result) ->
  Method s
command = Command . wrapJSON

-- | A Query can send a reply to the user, but it has a read-only view of the
-- server's state.
newtype Query s a
  = Q (ReaderT RequestID (ReaderT s IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

runQuery :: MonadIO m => Query s a -> RequestID -> s -> m a
runQuery q r s = liftIO (coerce q r s)

query ::
  forall params result s.
  (JSON.FromJSON params, JSON.ToJSON result) =>
  (params -> Query s result) ->
  Method s
query = Query . wrapJSON

nullQuery ::
  forall result s.
  (JSON.ToJSON result) =>
  Query s result ->
  Method s
nullQuery q = Query (\_ -> JSON.toJSON <$> q)

-- | A Notification can modify the server state, but it cannot reply to the user.
newtype Notification s a
  = N (StateT s IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runNotification :: MonadIO m => Notification s a -> s -> m (s, a)
runNotification n s = liftIO $
  do (a, s') <- coerce n s
     pure (s', a)

notification ::
  forall params s.
  (JSON.FromJSON params) =>
  (params -> Notification s ()) ->
  Method s
notification n =
  -- This implementation is similar to wrapJSON, but since we don't have a
  -- RequestID available, we can't quite copy that logic
  Notification $ \p -> N $ do
    case JSON.fromJSON @params p of
      JSON.Error msg ->
        liftIO $ throwIO (invalidParams p Nothing)
      JSON.Success params ->
        do runNotification (n params) =<< get
           pure ()

-- | Transform a command into a query by throwing away its state modifications
stateless :: (a -> Command s b) -> (a -> Query s b)
stateless c p =
  do r <- getRequestID
     (_, a) <- runCommand (c p) r =<< getState
     pure a

class IsMethod m where
  -- | The IsStateful type family lets us insert a custom type error for
  -- attempts to modify the state from within a Query
  type IsStateful m :: Constraint
  type IsStateful m = ()
  -- | The HasRequestID type family lets us insert a custom type error for
  -- attempts to get a request ID when there is none
  type HasRequestID m :: Constraint
  type HasRequestID m = ()
  -- | Get the current request ID
  getRequestID :: HasRequestID m => m s RequestID
  -- | Get the current server state
  getState     :: m s s
  -- | Alter the current server state (if this is permitted)
  modifyState  :: IsStateful m => (s -> s) -> m s ()

-- | Set the current server state (if this is permitted)
setState :: (IsMethod m, IsStateful m) => s -> m s ()
setState = modifyState . const

instance IsMethod Command where
  getRequestID  = C ask
  getState      = C get
  modifyState f = C (modify f)

instance IsMethod Query where
  getRequestID  = Q ask
  getState      = Q (lift (ask @_ @(ReaderT _ IO)))
  type IsStateful Query =
    TypeError
      ('Text "Methods in the ‘Query’ monad may not modify server state" :$$:
       'Text "Consider using the ‘Command’ or ‘Notification’ monad instead")
  modifyState = error $
    "Internal error: runtime attempt to modify server state inside a query " ++
    "(should be ruled out statically)"

instance IsMethod Notification where
  type HasRequestID Notification =
    TypeError
      ('Text "Methods in the ‘Notification’ monad aren't associated with a request ID" :$$:
       'Text "Consider using the ‘Command’ or ‘Query’ monad instead")
  getRequestID  = error $
    "Internal error: runtime attempt to get request ID inside a notification " ++
    "(should be ruled out statically)"
  getState      = N get
  modifyState f = N (modify f)

wrapJSON ::
  forall params result m s.
  (IsMethod m, HasRequestID m, MonadIO (m s),
   JSON.FromJSON params, JSON.ToJSON result) =>
  (params -> m s result) ->
  (JSON.Value -> m s JSON.Value)
wrapJSON c p =
  case JSON.fromJSON @params p of
    JSON.Error msg ->
      raise (invalidParams p . Just)
    JSON.Success params ->
      JSON.toJSON @result <$> c params

-- | Raise a 'JSONRPCException', flagging it for the client with the current
-- request ID, if there is one
raise ::
  (IsMethod m, HasRequestID m, MonadIO (m s)) =>
  (RequestID -> JSONRPCException) ->
  m s a
raise e =
  do rid <- getRequestID
     liftIO $ throwIO (e rid)

-- | An application is a state and a mapping from names to methods.
data App s =
  App { _appState :: MVar s
      , _appMethods :: Map Text (Method s)
      }

appState :: Simple Lens (App s) (MVar s)
appState = lens _appState (\a s -> a { _appState = s })

appMethods :: Simple Lens (App s) (Map Text (Method s))
appMethods = lens _appMethods (\a s -> a { _appMethods = s })

-- | Construct an application from an initial state and a mapping from method
-- names to methods.
mkApp ::
  s {- ^ the initial state -} ->
  [(Text, Method s)] {- ^ method names paired with their implementations -} ->
  IO (App s)
mkApp initState methods =
  App <$> newMVar initState <*> pure (M.fromList methods)

-- | JSON RPC exceptions should be thrown by method implementations when
-- they want to return an error.
data JSONRPCException =
  JSONRPCException { errorCode :: Integer
                     -- ^ The error code to be returned. From -32768 to -32000
                     -- is reserved by the protocol.
                   , message :: Text
                     -- ^ A single-sentence summary of the error
                   , errorData :: Maybe JSON.Value
                     -- ^ More error data that might be useful. @Nothing@ will
                     -- cause the field to be omitted.
                   , errorID :: Maybe RequestID
                     -- ^ The request ID, if one is available. @Nothing@ omits
                     -- it, because JSON-RPC defines a meaning for null.
                   }
  deriving Show

instance Exception JSONRPCException

instance JSON.ToJSON JSONRPCException where
  toJSON exn =
    JSON.object
      [ "jsonrpc" .= jsonRPCVersion
      , "id" .=
        case errorID exn of
          Nothing -> JSON.Null
          Just theID -> JSON.toJSON theID
      , "error" .=
        (JSON.object $ ["code" .= JSON.Number (fromInteger (errorCode exn))
                       , "message" .= JSON.String (message exn)] ++
                       (("data" .=) <$> maybeToList (errorData exn)))
      ]


{-
The spec defines some errors and reserves some error codes (from
-32768 to -32000) for its own use.
-}

-- | A method was provided with incorrect parameters
invalidParams ::  JSON.Value -> Maybe RequestID -> JSONRPCException
invalidParams params theID =
  JSONRPCException { errorCode = -32602
                   , message = "Invalid params"
                   , errorData = Just params
                   , errorID = theID
                   }

-- | The input string could not be parsed as JSON
parseError :: Text -> JSONRPCException
parseError msg =
  JSONRPCException { errorCode = -32700
                   , message   = "Parse error"
                   , errorData = Just (JSON.String msg)
                   , errorID   = Nothing
                   }

-- | The method called does not exist
methodNotFound :: JSON.ToJSON a => Maybe RequestID -> Maybe a -> JSONRPCException
methodNotFound theID meth =
  JSONRPCException { errorCode = -32601
                   , message   = "Method not found"
                   , errorData = JSON.toJSON <$> meth
                   , errorID   = theID
                   }

-- | The request issued is not valid
invalidRequest :: JSONRPCException
invalidRequest =
  JSONRPCException { errorCode = -32600
                   , message   = "Invalid request"
                   , errorData = Nothing
                   , errorID   = Nothing
                   }

-- | Some unspecified bad thing happened in the server
internalError :: JSONRPCException
internalError =
  JSONRPCException { errorCode = -32603
                   , message   = "Internal error"
                   , errorData = Nothing
                   , errorID   = Nothing
                   }

{-
A JSON-RPC request ID is:

    An identifier established by the Client that MUST contain a
    String, Number, or NULL value if included. If it is not included
    it is assumed to be a notification. The value SHOULD normally not
    be Null [1] and Numbers SHOULD NOT contain fractional parts
-}

-- | Request IDs come from clients, and are used to match responses
-- with commands.
data RequestID = IDText !Text | IDNum !Scientific | IDNull
  deriving (Eq, Ord, Show)

instance JSON.FromJSON RequestID where
  parseJSON (JSON.String str) = pure (IDText str)
  parseJSON (JSON.Number i)   = pure (IDNum i)
  parseJSON JSON.Null         = pure IDNull
  parseJSON other             = JSON.typeMismatch "Request ID" other

instance JSON.ToJSON RequestID where
  toJSON (IDText str) = JSON.String str
  toJSON (IDNum i)    = JSON.Number i
  toJSON IDNull       = JSON.Null

{-
Because the presence of null and the absence of the key are distinct,
we have both a null constructor and wrap it in a Maybe. When the
request ID is not present, the request is a notification and the field
is Nothing.
-}

data Request =
  Request { _requestMethod :: !Text
          , _requestID :: !(Maybe RequestID)
          , _requestParams :: !JSON.Value
          }
  deriving (Show)

requestMethod :: Simple Lens Request Text
requestMethod = lens _requestMethod (\r m -> r { _requestMethod = m })

requestID :: Simple Lens Request (Maybe RequestID)
requestID = lens _requestID (\r i -> r { _requestID = i })

requestParams :: Simple Lens Request JSON.Value
requestParams = lens _requestParams (\r m -> r { _requestParams = m })

suchThat :: HasCallStack => JSON.Parser a -> (a -> Bool) -> JSON.Parser a
suchThat parser pred =
  do res <- parser
     if pred res
       then return res
       else fail "invalid value"

instance JSON.FromJSON Request where
  parseJSON =
    JSON.withObject ("JSON-RPC " <> T.unpack jsonRPCVersion <> " request") $
    \o ->
      (o .: "jsonrpc" `suchThat` (== jsonRPCVersion)) *>
      (Request <$> o .: "method" <*> o .:! "id" <*> o .: "params")

instance JSON.ToJSON Request where
  toJSON req =
    JSON.object
      [ "jsonrpc" .= jsonRPCVersion
      , "method"  .= view requestMethod req
      , "id"      .= view requestID     req
      , "params"  .= view requestParams req ]
  toEncoding req =
    JSON.pairs $
      "jsonrpc" .= jsonRPCVersion         <>
      "method"  .= view requestMethod req <>
      "id"      .= view requestID     req <>
      "params"  .= view requestParams req

handleRequest :: forall s . (BS.ByteString -> IO ()) -> App s -> Request -> IO ()
handleRequest out app req =
  let method   = view requestMethod req
      params   = view requestParams req
      reqID    = view requestID req
      theState :: MVar s = view appState app
  in
    case M.lookup method $ view appMethods app of
      Nothing -> throwIO $ methodNotFound reqID (Just method)
      Just m ->
        case m of
          Command impl ->
            do rid <- requireID reqID
               answer <- modifyMVar theState (runCommand (impl params) rid)
               let response = JSON.object [ "jsonrpc" .= jsonRPCVersion
                                          , "id" .= reqID
                                          , "result" .= answer
                                          ]
               out (JSON.encode response)
          Query impl ->
            do rid <- requireID reqID
               answer <- runQuery (impl params) rid =<< readMVar theState
               let response = JSON.object [ "jsonrpc" .= jsonRPCVersion
                                          , "id" .= reqID
                                          , "result" .= answer
                                          ]
               out (JSON.encode response)
          Notification impl ->
            do requireNoID reqID
               modifyMVar theState (runNotification (impl params))
  where
    requireID (Just rid) = return rid
    requireID Nothing  = throwIO invalidRequest

    requireNoID (Just _) = throwIO invalidRequest
    requireNoID Nothing = return ()

-- | Given an IO action, return an atomic-ified version of that same action,
-- such that it closes over a lock. This is useful for synchronizing on output
-- to handles.
synchronized :: (a -> IO b) -> IO (a -> IO b)
synchronized f =
  do lock <- newMVar ()
     pure $ \a ->
       withMVar lock $ \_ -> f a

-- | One way to run a server is on stdio, listening for requests on stdin
-- and replying on stdout. In this system, each request must be on a
-- line for itself, and no newlines are otherwise allowed.
serveStdIO :: App s -> IO ()
serveStdIO = serveHandles stdin stdout

-- | Serve an application, listening for input on one handle and
-- sending output to another. Each request must be on a line for
-- itself, and no newlines are otherwise allowed.
serveHandles ::
  Handle {- ^ input handle    -} ->
  Handle {- ^ output handle   -} ->
  App s  {- ^ RPC application -} ->
  IO ()
serveHandles hIn hOut app = init >>= loop
  where
    newline = 0x0a -- ASCII/UTF8

    init = (,) <$> synchronized (BS.hPutStr hOut)
               <*> (BS.split newline <$> BS.hGetContents hIn)

    loop (out, input) =
      case input of
        [] -> return ()
        l:rest ->
          do forkIO $
               (case JSON.eitherDecode l of
                  Left msg -> throw (parseError (T.pack msg))
                  Right req -> handleRequest out app req)
               `catch` reportError out
               `catch` reportOtherException out
             loop (out, rest)

    reportError :: (BS.ByteString -> IO ()) -> JSONRPCException -> IO ()
    reportError out exn =
      out (JSON.encode exn <> BS.singleton newline)

    reportOtherException :: (BS.ByteString -> IO ()) -> SomeException -> IO ()
    reportOtherException = undefined  -- TODO: convert to JSONRPCException

-- | Serve an application on stdio, with messages encoded as netstrings.
serveStdIONS :: App s -> IO ()
serveStdIONS = serveHandlesNS Nothing stdin stdout

-- | Serve an application on arbitrary handles, with messages
-- encoded as netstrings.
serveHandlesNS ::
  Maybe Handle {- ^ logging handle -} ->
  Handle       {- ^ input handle    -} ->
  Handle       {- ^ output handle   -} ->
  App s        {- ^ RPC application -} ->
  IO ()
serveHandlesNS hLog hIn hOut app =
  do hSetBinaryMode hIn True
     hSetBuffering hIn NoBuffering
     input <- newMVar hIn
     output <- synchronized (\msg ->
                               do log msg
                                  BS.hPut hOut $ encodeNetstring $ netstring msg)
     loop output input
  where
    loop :: (BS.ByteString -> IO ()) -> MVar Handle -> IO ()
    loop output input =
      do line <- withMVar input $ netstringFromHandle
         log line
         forkIO $
               (case JSON.eitherDecode (decodeNetstring line) of
                  Left msg -> throwIO (parseError (T.pack msg))
                  Right req -> handleRequest output app req)
                 `catch` reportError output
                 -- TODO add a catch for other errors that throws a JSON-RPC wrapper
         loop output input

    reportError :: (BS.ByteString -> IO ()) -> JSONRPCException -> IO ()
    reportError output exn =
      output (JSON.encode exn)

    log :: Show a => a -> IO ()
    log =
      case hLog of
        Nothing -> const (return ())
        Just h -> hPutStrLn h . show

serveHTTP ::
  App s  {- JSON-RPC app -} ->
  Int    {- port number  -} ->
  IO ()
serveHTTP app port =
    scotty port $ post "/:whatevs" $
    do req <- request
       body <- liftIO $ strictRequestBody req
       -- NOTE: Making the assumption that WAI forks a thread - TODO: verify this
       stream $ \put flush ->
         do output <- synchronized (\ str -> put (fromByteString (BS.toStrict str)) *> flush)
            let reportError = \ (exn :: JSONRPCException) ->
                                output (JSON.encode exn <> BS.singleton newline)
            (case JSON.eitherDecode body of
               Left msg -> throw (parseError (T.pack msg))
               Right req -> handleRequest output app req)
             `catch` reportError
 where newline = 0x0a -- ASCII/UTF8
