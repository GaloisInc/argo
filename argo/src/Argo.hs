{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-name-shadowing #-}

-- | An implementation of the basic primitives of JSON-RPC 2.0.
module Argo
  ( -- * Primary interface to JSON-RPC
    App
  , mkApp
  -- * Defining methods
  , MethodType(..)
  , Method(..)
  , runMethod
  , method
  -- * Manipulating state in methods
  , getState
  , modifyState
  , setState
  -- * File I/O in methods that respects caching
  , getFileReader
  -- * "printf"-style debugging in methods
  , getDebugLogger
  , debugLog
  -- * JSON-RPC exceptions: The spec defines some errors and reserves some error
  -- codes (from -32768 to -32000) for its own use.
  , JSONRPCException
  , makeJSONRPCException
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
  , serveHTTP
  -- * Request identifiers
  , RequestID(..)
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception hiding (TypeError)
import Control.Lens hiding ((.=))
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.:!), (.=))
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)
import Data.Binary.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import Network.Wai (strictRequestBody)
import System.IO
import System.IO.Silently
import Web.Scotty hiding (raise, params, get, put)

import Argo.ServerState
import Argo.Netstring

type AppServerState = ServerState (Text, JSON.Object)

-- | We only support JSON-RPC 2.0.
jsonRPCVersion :: Text
jsonRPCVersion = "2.0"

-- | A server has /state/, and a collection of (potentially) stateful /methods/,
-- each of which is a function from the JSON value representing its parameters
-- to a JSON value representing its response.
newtype Method state result
  = Method (ReaderT (Text -> IO (), FilePath -> IO B.ByteString) (StateT state IO) result)
  deriving (Functor, Applicative, Monad, MonadIO)

runMethod :: Method state result -> (FilePath -> IO B.ByteString) -> (Text -> IO ()) -> state -> IO (state, result)
runMethod (Method m) reader log s = flop <$> runStateT (runReaderT m (log, reader)) s
  where flop (x, y) = (y, x)



-- | A 'Method' may be one of three different sorts
data MethodType
  = Command  -- ^ can modify state and can reply to the client
  | Query    -- ^ can /not/ modify state, but can reply to the client
  | Notification  -- ^ can modify state, but can /not/ reply to the client
  deriving (Eq, Ord, Show)

-- | Given an arbitrary 'Method', wrap its input and output in JSON
-- serialization. Note that because the JSON representation of a 'JSON.Value' is
-- itself, you can manipulate raw JSON inputs/outputs by using 'JSON.Value' as a
-- parameter or result type. The resultant 'Method' may throw an 'invalidParams'
-- exception.
method ::
  forall params result state.
  (JSON.FromJSON params, JSON.ToJSON result) =>
  (params -> Method state result) ->
  (JSON.Value -> Method state JSON.Value)
method f p =
  case JSON.fromJSON @params p of
    JSON.Error msg -> raise $ invalidParams msg p
    JSON.Success params -> JSON.toJSON <$> f params

-- | Get the logger from the server
getDebugLogger :: Method state (Text -> IO ())
getDebugLogger = Method (asks fst)

-- | Log a message for debugging
debugLog :: Text -> Method state ()
debugLog message =
  do logger <- getDebugLogger
     liftIO $ logger message

-- | Get the file reader from the server
getFileReader :: Method state (FilePath -> IO B.ByteString)
getFileReader = Method (asks snd)

-- | Get the state of the server
getState :: Method state state
getState = Method get

-- | Modify the state of the server with some function
modifyState :: (state -> state) -> Method state ()
modifyState f = Method (modify f)

-- | Set the state of the server
setState :: state -> Method state ()
setState = Method . put

-- | Raise a 'JSONRPCException' within a 'Method'
raise :: JSONRPCException -> Method state a
raise = liftIO . throwIO

--------------------------------------------------------------------------------

-- | An application is a state and a mapping from names to methods.
data App s =
  App { _serverState :: MVar (AppServerState s)
      , _appMethods :: Map Text (MethodType, JSON.Value -> Method s JSON.Value)
      }

-- | Focus on the state var in an 'App'
serverState :: Lens' (App s) (MVar (AppServerState s))
serverState = lens _serverState (\a s -> a { _serverState = s })

-- | Focus on the 'Method's in an 'App'
appMethods ::
  Lens' (App s) (Map Text (MethodType, JSON.Value -> Method s JSON.Value))
appMethods = lens _appMethods (\a s -> a { _appMethods = s })

-- | Construct an application from an initial state and a mapping from method
-- names to methods.
mkApp ::
  ((FilePath -> IO B.ByteString) -> IO s) {- ^ how to get the initial state -} ->
  [(Text, MethodType, JSON.Value -> Method s JSON.Value)]
  {- ^ method names paired with their implementations -} ->
  IO (App s)
mkApp initAppState methods =
  App <$> (newMVar =<< initialState initAppState)
      <*> pure (M.fromList [ (k, (v1, v2)) | (k, v1, v2) <- methods])

-- | JSON RPC exceptions should be thrown by method implementations when
-- they want to return an error.
data JSONRPCException =
  JSONRPCException
    { errorCode :: Integer
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
    , errorStdOut :: String
      -- ^ The standard output of the method producing this error
    , errorStdErr :: String
      -- ^ The standard error of the method producing this error
    } deriving Show

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
                      , "message" .= JSON.String (message exn)
                      , "data" .= (JSON.object $
                        ([ "stdout" .= errorStdOut exn
                         , "stderr" .= errorStdErr exn ] ++
                         (("data" .=) <$> maybeToList (errorData exn))))
                      ])
      ]

-- | A method was provided with incorrect parameters (from the JSON-RPC spec)
invalidParams :: String -> JSON.Value -> JSONRPCException
invalidParams msg params =
  JSONRPCException { errorCode = -32602
                   , message = T.pack ("Invalid params: " ++ msg)
                   , errorData = Just params
                   , errorID = Nothing
                   , errorStdOut = ""
                   , errorStdErr = ""
                   }

-- | The input string could not be parsed as JSON (from the JSON-RPC spec)
parseError :: Text -> JSONRPCException
parseError msg =
  JSONRPCException { errorCode = -32700
                   , message   = "Parse error"
                   , errorData = Just (JSON.String msg)
                   , errorID   = Nothing
                   , errorStdOut = ""
                   , errorStdErr = ""
                   }

-- | The method called does not exist (from the JSON-RPC spec)
methodNotFound :: Text -> JSONRPCException
methodNotFound meth =
  JSONRPCException { errorCode = -32601
                   , message   = "Method not found"
                   , errorData = Just (JSON.toJSON meth)
                   , errorID   = Nothing
                   , errorStdOut = ""
                   , errorStdErr = ""
                   }

-- | The request issued is not valid (from the JSON-RPC spec)
invalidRequest :: JSONRPCException
invalidRequest =
  JSONRPCException { errorCode = -32600
                   , message   = "Invalid request"
                   , errorData = Nothing
                   , errorID   = Nothing
                   , errorStdOut = ""
                   , errorStdErr = ""
                   }

-- | Some unspecified bad thing happened in the server (from the JSON-RPC spec)
internalError :: JSONRPCException
internalError =
  JSONRPCException { errorCode = -32603
                   , message   = "Internal error"
                   , errorData = Nothing
                   , errorID   = Nothing
                   , errorStdOut = ""
                   , errorStdErr = ""
                   }

-- | The provided State ID is not one that the server has previously sent.
unknownStateID :: StateID -> JSONRPCException
unknownStateID sid =
  JSONRPCException { errorCode = 20
                   , message = "Unknown state ID"
                   , errorData = Just $ JSON.toJSON sid
                   , errorID = Nothing
                   , errorStdOut = ""
                   , errorStdErr = ""
                   }

-- | Construct a 'JSONRPCException' from an error code, error text, and perhaps
-- some data item to attach
makeJSONRPCException :: JSON.ToJSON a => Integer -> Text -> Maybe a -> JSONRPCException
makeJSONRPCException c m d =
  JSONRPCException
    { errorCode = c
    , message = m
    , errorData = JSON.toJSON <$> d
    , errorID = Nothing
    , errorStdOut = ""
    , errorStdErr = ""
    }


{- | Request IDs come from clients, and are used to match responses
with commands.

Per the spec, a JSON-RPC request ID is: An identifier established by the Client
that MUST contain a String, Number, or NULL value if included. If it is not
included it is assumed to be a notification. The value SHOULD normally not be
Null [1] and Numbers SHOULD NOT contain fractional parts
-}
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

data Request =
  Request { _requestMethod :: !Text
          -- ^ The method name to invoke
          , _requestID :: !(Maybe RequestID)
          -- ^ Because the presence of null and the absence of the key are
          -- distinct, we have both a null constructor and wrap it in a Maybe.
          -- When the request ID is not present, the request is a notification
          -- and the field is Nothing.
          , _requestParams :: !JSON.Object
          -- ^ The parameters to the method, if any exist
          }
  deriving (Show)

requestMethod :: Lens' Request Text
requestMethod = lens _requestMethod (\r m -> r { _requestMethod = m })

requestID :: Lens' Request (Maybe RequestID)
requestID = lens _requestID (\r i -> r { _requestID = i })

requestParams :: Lens' Request JSON.Object
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

rerunStep ::
  forall s.
  Maybe RequestID -> App s -> (Text -> IO ()) ->
  s -> (Text, JSON.Object) -> (FilePath -> IO B.ByteString) -> IO s
rerunStep reqID app logger startState (method, params) fileReader =
  case M.lookup method $ view appMethods app of
    Nothing -> throwIO $ (methodNotFound method) { errorID = reqID }
    Just (_, m) ->
      fst <$> runMethod (m $ JSON.Object params) fileReader logger startState

-- | A response to a command or query.
--
-- The first type parameter represents the type of state IDs used at
-- the particular stage of the program. When a response is first
-- constructed, the state ID does not yet exist, so the type parameter
-- is (). It becomes 'StateID' later, once the protocol expects this
-- to exist, and the lack of a generalized 'ToJSON' instance prevents
-- us from forgetting the ID when we encode it.
data Response stateID a
  = Response { _responseID :: !RequestID
             , _responseAnswer :: !a
             , _responseStdOut :: !String
             , _responseStdErr :: !String
             , _responseStateID :: !stateID
             } deriving (Eq, Ord, Show)

responseID :: Lens' (Response stateID a) RequestID
responseID = lens _responseID (\r m -> r { _responseID = m })

responseAnswer :: Lens (Response stateID a) (Response stateID b) a b
responseAnswer = lens _responseAnswer (\r m -> r { _responseAnswer = m })

responseStdOut :: Lens' (Response stateID a) String
responseStdOut = lens _responseStdOut (\r m -> r { _responseStdOut = m })

responseStdErr :: Lens' (Response stateID a) String
responseStdErr = lens _responseStdOut (\r m -> r { _responseStdOut = m })

responseStateID :: Lens (Response stateID a) (Response stateID' a) stateID stateID'
responseStateID = lens _responseStateID (\r sid -> r { _responseStateID = sid })

instance JSON.FromJSON a => JSON.FromJSON (Response StateID a) where
  parseJSON =
    JSON.withObject ("JSON-RPC" <> T.unpack jsonRPCVersion <> " response") $
    \o ->
      (o .: "jsonrpc" `suchThat` (== jsonRPCVersion)) *>
      (do result <- o .: "result"
          Response
            <$> o .: "id"
            <*> result .: "answer"
            <*> result .: "stdout"
            <*> result .: "stderr"
            <*> result .: "state")

instance JSON.ToJSON a => JSON.ToJSON (Response StateID a) where
  toJSON resp =
    JSON.object
      [ "jsonrpc" .= jsonRPCVersion
      , "id"      .= view responseID resp
      , "result"  .= JSON.object
        [ "answer" .= view responseAnswer resp
        , "stdout" .= view responseStdOut resp
        , "stderr" .= view responseStdErr resp
        , "state"  .= view responseStateID resp
        ]
      ]

runMethodResponse ::
  Method s a ->
  RequestID ->
  (FilePath -> IO B.ByteString) ->
  (Text -> IO ()) ->
  s ->
  IO (s, Response () a)
execMethodSilently ::
  Method s a ->
  (FilePath -> IO B.ByteString) ->
  (Text -> IO ()) ->
  s ->
  IO s
(runMethodResponse, execMethodSilently) =
  ( \m reqID fileReader logger st ->
      tryOutErr hCapture reqID (runMethod m fileReader logger st)
  , \m fileReader logger st -> fst <$>
      tryOutErr
        (\hs a -> ("",) <$> hSilence hs a)
        IDNull
        (runMethod m fileReader logger st) )
  where
    tryOutErr ::
      (forall a. [Handle] -> IO a -> IO (String, a)) ->
      RequestID ->
      IO (s, a) ->
      IO (s, Response () a)
    tryOutErr capturer reqID action =
      do (err, (out, res)) <-
           capturer [stderr] . capturer [stdout] $
           try @SomeException (try @JSONRPCException action)
         tryPutOutErr out err
         either
           (\e -> let message = Just (JSON.String (T.pack (displayException e)))
                  in throwIO $ internalError { errorData = message })
           (either
              (\e -> throwIO $ e { errorStdOut = out, errorStdErr = err })
              (\(st, answer) -> pure (st, Response reqID answer out err ())))
           res
      where
        tryPutOutErr :: String -> String -> IO ()
        tryPutOutErr out err =
          do void $ try @IOException (hPutStr stdout out)
             void $ try @IOException (hPutStr stderr err)

handleRequest ::
  forall s.
  HasCallStack =>
  (Text -> IO ()) ->
  (BS.ByteString -> IO ()) ->
  App s ->
  Request ->
  IO ()
handleRequest logger respond app req =
  case M.lookup method $ view appMethods app of
    Nothing -> throwIO $ (methodNotFound method) { errorID = reqID }
    Just (Command, m) ->
      withRequestID $ \reqID ->
      do stateID <- getStateID req
         response <- withMVar theState $
           \contents ->
             getAppState (rerunStep (Just reqID) app logger) contents stateID >>=
             \case
               Nothing -> throwIO $ unknownStateID stateID
               Just initAppState ->
                 do let r = (method, view requestParams req)
                    let fileReader = freshStateReader contents stateID
                    (newAppState, result) <- runMethodResponse (m params) reqID fileReader logger initAppState
                    sid' <- saveNewState contents stateID r newAppState
                    return $ addStateID sid' result
         respond (JSON.encode response)
    Just (Query, m) ->
      withRequestID $ \reqID ->
      do stateID <- getStateID req
         response <- withMVar theState $
           \contents ->
             getAppState (rerunStep (Just reqID) app logger) contents stateID >>=
             \case
               Nothing -> throwIO $ unknownStateID stateID
               Just theAppState ->
                 do let fileReader = B.readFile -- No caching of this state
                    (_, result) <- runMethodResponse (m params) reqID fileReader logger theAppState
                    -- Here, we return the original state ID, because
                    -- queries don't result in new states.
                    return $ addStateID stateID result
         respond (JSON.encode response)
    Just (Notification, m) ->
      withoutRequestID $
      void $
      do stateID <- getStateID req
         withMVar theState $
           \contents ->
             getAppState (rerunStep reqID app logger) contents stateID >>=
             \case
               Nothing -> throwIO $ unknownStateID stateID
               Just theAppState ->
                 -- No reply will be sent, so there is no way of the
                 -- client re-using any resulting state. So we don't
                 -- cache file contents here.
                 (,()) <$> execMethodSilently (m params) B.readFile logger theAppState
  where
    method   = view requestMethod req
    params   = JSON.Object $ view requestParams req
    reqID    = view requestID req
    theState = view serverState app

    addStateID :: StateID -> Response anyOldStateID a -> Response StateID a
    addStateID sid answer =
      set responseStateID sid answer

    withRequestID :: (RequestID -> IO a) -> IO a
    withRequestID act =
      do rid <- requireID
         catch (act rid) $ \e -> throwIO (e { errorID = Just rid })

    withoutRequestID :: IO a -> IO a
    withoutRequestID act =
      do requireNoID
         catch act $ \e -> throwIO (e { errorID = Nothing })

    requireID   = maybe (throwIO invalidRequest) return reqID
    requireNoID = maybe (return ()) (const (throwIO invalidRequest)) reqID


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
serveStdIO :: HasCallStack => App s -> IO ()
serveStdIO = serveHandles (Just stderr) stdin stdout

getStateID :: Request -> IO StateID
getStateID req =
  case view (requestParams . at "state") req of
    Just sid ->
      case JSON.fromJSON sid of
        JSON.Success i -> pure i
        JSON.Error msg -> noStateID msg
    Nothing -> noStateID "No state field in parameters"
  where
    noStateID msg =
      throwIO $
        invalidParams msg $ JSON.Object $ view requestParams req


-- | Serve an application, listening for input on one handle and
-- sending output to another. Each request must be on a line for
-- itself, and no newlines are otherwise allowed.
serveHandles ::
  HasCallStack =>
  Maybe Handle {- ^ Where to log debug info -} ->
  Handle {- ^ input handle    -} ->
  Handle {- ^ output handle   -} ->
  App s  {- ^ RPC application -} ->
  IO ()
serveHandles hLog hIn hOut app = init >>= loop
  where
    newline = 0x0a -- ASCII/UTF8

    init = (,) <$> synchronized (BS.hPutStr hOut)
               <*> (BS.split newline <$> BS.hGetContents hIn)

    loop (out, input) =
      case input of
        [] -> return ()
        l:rest ->
          do _ <- forkIO $
               (case JSON.eitherDecode l of
                  Left msg -> throw (parseError (T.pack msg))
                  Right req -> handleRequest log out app req)
               `catch` reportError out
               `catch` reportOtherException out
             loop (out, rest)

    reportError :: (BS.ByteString -> IO ()) -> JSONRPCException -> IO ()
    reportError out exn =
      out (JSON.encode exn <> BS.singleton newline)

    reportOtherException :: (BS.ByteString -> IO ()) -> SomeException -> IO ()
    reportOtherException out exn =
      out $
        JSON.encode (internalError { errorData = Just (JSON.String (T.pack (show exn))) })
        <> BS.singleton newline

    log = case hLog of
            Just h -> T.hPutStrLn h
            Nothing -> const (return ())

-- | Serve an application on stdio, with messages encoded as netstrings.
serveStdIONS :: HasCallStack => App s -> IO ()
serveStdIONS = serveHandlesNS (Just stderr) stdin stdout

-- | Serve an application on arbitrary handles, with messages
-- encoded as netstrings.
serveHandlesNS ::
  HasCallStack =>
  Maybe Handle {- ^ debug log handle -} ->
  Handle       {- ^ input handle     -} ->
  Handle       {- ^ output handle    -} ->
  App s        {- ^ RPC application  -} ->
  IO ()
serveHandlesNS hLog hIn hOut app =
  do hSetBinaryMode hIn True
     hSetBuffering hIn NoBuffering
     input <- newMVar hIn
     output <- synchronized (\msg ->
                               do log (T.pack (show msg))
                                  BS.hPut hOut $ encodeNetstring $ netstring msg)
     loop output input
  where
    loop :: (BS.ByteString -> IO ()) -> MVar Handle -> IO ()
    loop output input =
      do mbLine <- withMVar input $ netstringFromHandle
         case mbLine of
           Nothing   -> return ()
           Just line ->
             do _ <- processLine output line
                loop output input

    processLine output line =
      do log (T.pack (show line))
         forkIO $
           case JSON.eitherDecode (decodeNetstring line) of
             Left  msg -> throwIO (parseError (T.pack msg))
             Right req -> handleRequest log output app req
           `catch` reportError output
           `catch` reportOtherException output

    reportError :: (BS.ByteString -> IO ()) -> JSONRPCException -> IO ()
    reportError output exn =
      output (JSON.encode exn)

    reportOtherException :: (BS.ByteString -> IO ()) -> SomeException -> IO ()
    reportOtherException out exn =
      out $ JSON.encode $
      internalError { errorData = Just (JSON.String (T.pack (show exn)))
                    , errorStdOut = "<no stdout captured: exception outside capture region>"
                    , errorStdErr = "<no stderr captured: exception outside capture region>"
                    }

    log :: Text -> IO ()
    log msg = case hLog of
                Just h -> T.hPutStrLn h msg
                Nothing -> return ()


serveHTTP ::
  HasCallStack =>
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
            let reportError =
                  \ (exn :: JSONRPCException) ->
                    output (JSON.encode exn <> BS.singleton newline)
                reportOtherException =
                  \ (exn :: SomeException) ->
                    output $ JSON.encode (internalError { errorData = Just (JSON.String (T.pack (show exn))) })
                             <> BS.singleton newline
            (case JSON.eitherDecode body of
               Left msg -> throw (parseError (T.pack msg))
               Right req -> handleRequest (T.hPutStrLn stderr) output app req)
             `catch` reportError
             `catch` reportOtherException
 where newline = 0x0a -- ASCII/UTF8
