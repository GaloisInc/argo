{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , appName
  , appDocumentation
  , defaultAppOpts
  , StateMutability(..)
  , AppOpts
  -- * Method Options
  , FileSystemMode(..)
  , MethodOptions(..)
  , defaultMethodOptions
  -- * Defining methods
  , AppMethod
  , Method
  , StatefulMethod
  , Command
  , Query
  , Notification
  , command
  , query
  , notification
  -- * Manipulating app state in methods
  , getState
  , modifyState
  , setState
  -- * Manipulating internal server state in notifications
  , destroyState
  , destroyAllStates
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
  , serveHttp
  -- * Request identifiers
  , RequestID(..)
  , StateID
  -- * System info
  , getFileSystemMode
  -- * AppMethod info
  , methodName
  , methodParamDocs
  , methodReturnFieldDocs
  , methodDocs
  -- * HTTP options
  , HttpOptions(..)
  , tlsEnvVar
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception hiding (TypeError)
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.:!), (.=))
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Maybe (maybeToList)
import Data.Scientific (Scientific)
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Stack
import Numeric.Natural ( Natural )
import Network.HTTP.Types.Method (StdMethod(..))
import Network.HTTP.Types.Status
import System.IO
import System.IO.Silently ( hCapture, hSilence )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty
    ( addroute,
      body,
      finish,
      header,
      literal,
      post,
      raw,
      scotty,
      setHeader,
      status,
      text )
import Web.Scotty.TLS (scottyTLS)

import qualified Argo.Doc as Doc
import Argo.ServerState
import Argo.Netstring

-- | We only support JSON-RPC 2.0.
jsonRPCVersion :: Text
jsonRPCVersion = "2.0"

--------------------------------------------------------------------------------

-- | Options affecting the how the server runs/supports the app.
newtype AppOpts =
  AppOpts
  {
    appStateMutability :: StateMutability
    -- ^ Whether or not the underlying application state is mutable
  }

defaultAppOpts :: StateMutability -> AppOpts
defaultAppOpts stateMut = AppOpts { appStateMutability = stateMut}

data FileSystemMode
  = ReadOnly
  | ReadWrite
  deriving (Eq)

data MethodOptions =
  MethodOptions
    { optFileSystemMode :: FileSystemMode
    -- ^ Allowable behavior with respect to the external file system.
    -- ReadOnly prohibits creating or writing files.
    , optLogger :: Text -> IO ()
    -- ^ A function to use for logging debug messages.
    , optMaxOccupancy :: Natural
    -- ^ How many simultaneous states can be live at once?
    }

defaultMethodOptions :: MethodOptions
defaultMethodOptions =
  MethodOptions
    { optFileSystemMode = ReadWrite
    , optLogger = const (return ())
    , optMaxOccupancy = 10
    }

data CommandContext =
  CommandContext
  { cmdCtxMOptions :: MethodOptions
  , cmdCtxFileReader :: FilePath -> IO B.ByteString
  }

data QueryContext =
  QueryContext
  { queryCtxMOptions :: MethodOptions
  , queryCtxFileReader :: FilePath -> IO B.ByteString
  }

data NotificationContext =
  NotificationContext
  { ntfCtxMOptions :: MethodOptions
  , ntfCtxFileReader :: FilePath -> IO B.ByteString
  -- ^ A function returning the contents of the named file,
  -- potentially from a cache.
  , ntfCtxDestroyState :: StateID -> IO ()
  -- ^ A function for unpinning a state (i.e., removing it from the persistent cache).
  , ntfCtxDestroyAllStates :: IO ()
  -- ^ A function for unpinning all pinned states (i.e., clearing the persistent cache).
  }


--------------------------------------------------------------------------------

-- | A server has /state/, and a collection of (potentially) stateful /methods/,
-- each of which is a function from the JSON value representing its parameters
-- to a JSON value representing its response.
newtype Command state result
  = Command (ReaderT CommandContext (StateT state IO) result)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState state)

runCommand :: Command state result -> CommandContext -> state -> IO (state, result)
runCommand (Command m) cctx s = flop <$> runStateT (runReaderT m cctx) s
  where flop (x, y) = (y, x)

newtype Query state result
  = Query (ReaderT (QueryContext, state) IO result)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (QueryContext, state))

runQuery :: Query state result -> QueryContext -> state -> IO result
runQuery (Query m) qctx s = runReaderT m (qctx, s)

newtype Notification result
  = Notification (ReaderT NotificationContext IO result)
  deriving (Functor, Applicative, Monad, MonadIO)

runNotification :: Notification () -> NotificationContext -> IO ()
runNotification (Notification m) mctx = void $ runReaderT m mctx


data AppCommand appState =
  AppCommand
  { commandName :: !Text
  , commandImplementation :: !(JSON.Value -> Command appState JSON.Value)
  , commandParamDocs :: ![(Text, Doc.Block)]
  , commandReturnFieldDocs :: ![(Text, Doc.Block)]
  , commandDocs :: !Doc.Block
  }

data AppQuery appState =
  AppQuery
  { queryName :: !Text
  , queryImplementation :: !(JSON.Value -> Query appState JSON.Value)
  , queryParamDocs :: ![(Text, Doc.Block)]
  , queryReturnFieldDocs :: ![(Text, Doc.Block)]
  , queryDocs :: !Doc.Block
  }

data AppNotification =
  AppNotification
  { notificationName :: !Text
  , notificationImplementation :: !(JSON.Value -> Notification ())
  , notificationParamDocs :: ![(Text, Doc.Block)]
  , notificationDocs :: !Doc.Block
  }

data AppMethod appState
  = CommandMethod (AppCommand appState)
  | QueryMethod (AppQuery appState)
  | NotificationMethod AppNotification

methodName :: AppMethod appState -> Text
methodName (CommandMethod m) = commandName m
methodName (QueryMethod m) = queryName m
methodName (NotificationMethod m) = notificationName m

methodKind :: AppMethod appState -> Text
methodKind CommandMethod{} = "command"
methodKind QueryMethod{} = "query"
methodKind NotificationMethod{} = "notification"

methodParamDocs :: AppMethod appState -> [(Text, Doc.Block)]
methodParamDocs (CommandMethod m) = commandParamDocs m
methodParamDocs (QueryMethod m) = queryParamDocs m
methodParamDocs (NotificationMethod m) = notificationParamDocs m

methodReturnFieldDocs :: AppMethod appState -> [(Text, Doc.Block)]
methodReturnFieldDocs (CommandMethod m) = commandReturnFieldDocs m
methodReturnFieldDocs (QueryMethod m) = queryReturnFieldDocs m
methodReturnFieldDocs NotificationMethod{} = []

methodDocs :: AppMethod appState -> Doc.Block
methodDocs (CommandMethod m) = commandDocs m
methodDocs (QueryMethod m) = queryDocs m
methodDocs (NotificationMethod m) = notificationDocs m


-- | Given an arbitrary 'Command', wrap its input and output in JSON
-- serialization. Note that because the JSON representation of a 'JSON.Value' is
-- itself, you can manipulate raw JSON inputs/outputs by using 'JSON.Value' as a
-- parameter or result type. The resultant 'Command' may throw an 'invalidParams'
-- exception.
command ::
  forall params result state.
  (JSON.FromJSON params, Doc.DescribedMethod params result, JSON.ToJSON result) =>
  Text ->
  Doc.Block ->
  (params -> Command state result) ->
  AppMethod state
command name doc f =
  let impl =
        \p ->
         case JSON.fromJSON @params p of
           JSON.Error msg -> raise $ invalidParams msg p
           JSON.Success params -> JSON.toJSON <$> f params
  in CommandMethod $ AppCommand
     { commandName = name
     , commandImplementation = impl
     , commandParamDocs = Doc.parameterFieldDescription @params
     , commandReturnFieldDocs = Doc.resultFieldDescription @params @result
     , commandDocs = doc
     }

-- | Given an arbitrary 'Command', wrap its input and output in JSON
-- serialization. Note that because the JSON representation of a 'JSON.Value' is
-- itself, you can manipulate raw JSON inputs/outputs by using 'JSON.Value' as a
-- parameter or result type. The resultant 'Command' may throw an 'invalidParams'
-- exception.
query ::
  forall params result state.
  (JSON.FromJSON params, Doc.DescribedMethod params result, JSON.ToJSON result) =>
  Text ->
  Doc.Block ->
  (params -> Query state result) ->
  AppMethod state
query name doc f =
  let impl =
        \p ->
         case JSON.fromJSON @params p of
           JSON.Error msg -> raise $ invalidParams msg p
           JSON.Success params -> JSON.toJSON <$> f params
  in QueryMethod $ AppQuery
     { queryName = name
     , queryImplementation = impl
     , queryParamDocs = Doc.parameterFieldDescription @params
     , queryReturnFieldDocs = Doc.resultFieldDescription @params @result
     , queryDocs = doc
     }

-- | Given an arbitrary 'Notification', wrap its input in JSON serialization.
-- Note that because the JSON representation of a 'JSON.Value' is
-- itself, you can manipulate raw JSON inputs/outputs by using 'JSON.Value' as a
-- parameter or result type. The resultant 'Notification' may throw an 'invalidParams'
-- exception.
notification ::
  forall params state.
  (JSON.FromJSON params, Doc.DescribedMethod params ()) =>
  Text ->
  Doc.Block ->
  (params -> Notification ()) ->
  AppMethod state
notification name doc f =
  let impl =
        \p ->
         case JSON.fromJSON @params p of
           JSON.Error msg -> raise $ invalidParams msg p
           JSON.Success params -> f params
  in NotificationMethod $ AppNotification
     { notificationName = name
     , notificationImplementation = impl
     , notificationParamDocs = Doc.parameterFieldDescription @params
     , notificationDocs = doc
     }

class Method m where
  -- | Get the logger from the server
  getDebugLogger :: m (Text -> IO ())
  -- | Log a message for debugging
  debugLog :: Text -> m ()
  -- | Get the file reader from the server
  getFileReader :: m (FilePath -> IO B.ByteString)
  -- | Get the file system mode from the server
  getFileSystemMode :: m FileSystemMode
  -- | Raise a 'JSONRPCException' within a 'Method'
  raise :: JSONRPCException -> m a

instance Method (Command state) where
  getDebugLogger = Command (asks (optLogger . cmdCtxMOptions))
  debugLog message = do
    logger <- getDebugLogger
    liftIO $ logger message
  getFileReader = Command (asks cmdCtxFileReader)
  getFileSystemMode = Command (asks (optFileSystemMode . cmdCtxMOptions))
  raise = liftIO . throwIO

instance Method (Query state) where
  getDebugLogger = Query (asks (optLogger . queryCtxMOptions . fst))
  debugLog message = do
    logger <- getDebugLogger
    liftIO $ logger message
  getFileReader = Query (asks (queryCtxFileReader . fst))
  getFileSystemMode = Query (asks (optFileSystemMode . queryCtxMOptions . fst))
  raise = liftIO . throwIO

instance Method Notification where
  getDebugLogger = Notification (asks (optLogger . ntfCtxMOptions))
  debugLog message = do
    logger <- getDebugLogger
    liftIO $ logger message
  getFileReader = Notification (asks ntfCtxFileReader)
  getFileSystemMode = Notification (asks (optFileSystemMode . ntfCtxMOptions))
  raise = liftIO . throwIO


class StatefulMethod s m where
  -- | Get the app state associated with a stateful method invocation
  getState :: m s

instance StatefulMethod s (Command s) where
  getState = Command get

instance StatefulMethod s (Query s) where
  getState = Query $ asks snd


-- | Modify the state of the server with some function
modifyState :: (s -> s) -> Command s ()
modifyState f = Command (modify f)

-- | Set the state of the server
setState :: s -> Command s ()
setState = Command . put


-- | Destroy a state.
destroyState :: StateID -> Notification ()
destroyState sid = Notification $ do
  destroy <- asks ntfCtxDestroyState
  liftIO $ destroy sid


-- | Destroy all states.
destroyAllStates :: Notification ()
destroyAllStates = Notification $ do
  destroyAll <- asks ntfCtxDestroyAllStates
  liftIO $ destroyAll


--------------------------------------------------------------------------------

-- | An application is a state and a mapping from names to methods.
data App s =
  App
  { serverState :: MVar (ServerState s)
  , appMethods :: Map Text (AppMethod s)
  , appName :: Text
  , appDocumentation :: [Doc.Block]
  }


-- | Construct an application from an initial state and a mapping from method
-- names to methods. This app's state is assumed to be pure.
mkApp ::
  Text {- ^ Application name -} ->
  [Doc.Block] {- ^ Documentation -} ->
  AppOpts {- ^ App options to use at launch affecting how the server operates. -} ->
  ((FilePath -> IO B.ByteString) -> IO s) {- ^ how to get the initial state -} ->
  [AppMethod s]
  {- ^ Individual methods -} ->
  IO (App s)
mkApp name docs opts initAppState methods = do
  initialState <- newMVar =<< initServerState (appStateMutability opts) initAppState
  pure $ App
    { serverState = initialState
    , appMethods = M.fromList [ (methodName m, m)
                              | m <- methods]
    , appName = name
    , appDocumentation = appDocs
    }
  where appDocs =
          docs ++
          [Doc.Section "Methods"
            [ Doc.Section (methodName m <> " (" <> methodKind m <> ")")
                [ methodDocs m
                , Doc.Section "Parameter fields"
                  [ if null (methodParamDocs m)
                    then Doc.Paragraph [Doc.Text "No parameters"]
                    else Doc.DescriptionList
                           [ (Doc.Literal field :| [], fieldDocs)
                           | (field, fieldDocs) <- methodParamDocs m
                           ]
                  ]
                , Doc.Section "Return fields"
                  [ if null (methodReturnFieldDocs m)
                    then Doc.Paragraph [Doc.Text "No return fields"]
                    else Doc.DescriptionList
                           [ (Doc.Literal field :| [], fieldDocs)
                           | (field, fieldDocs) <- methodReturnFieldDocs m
                           ]
                  ]
                ]
            | m <- methods
            ]]


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
    , errorStdOut :: Maybe String
      -- ^ The standard output of the method producing this error
    , errorStdErr :: Maybe String
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
                   , errorStdOut = Nothing
                   , errorStdErr = Nothing
                   }

-- | The input string could not be parsed as JSON (from the JSON-RPC spec)
parseError :: Text -> JSONRPCException
parseError msg =
  JSONRPCException { errorCode = -32700
                   , message   = "Parse error"
                   , errorData = Just (JSON.String msg)
                   , errorID   = Nothing
                   , errorStdOut = Nothing
                   , errorStdErr = Nothing
                   }

-- | The method called does not exist (from the JSON-RPC spec)
methodNotFound :: Text -> JSONRPCException
methodNotFound meth =
  JSONRPCException { errorCode = -32601
                   , message   = "Method not found"
                   , errorData = Just (JSON.toJSON meth)
                   , errorID   = Nothing
                   , errorStdOut = Nothing
                   , errorStdErr = Nothing
                   }

-- | The request issued is not valid (from the JSON-RPC spec)
invalidRequest :: JSONRPCException
invalidRequest =
  JSONRPCException { errorCode = -32600
                   , message   = "Invalid request"
                   , errorData = Nothing
                   , errorID   = Nothing
                   , errorStdOut = Nothing
                   , errorStdErr = Nothing
                   }

-- | Some unspecified bad thing happened in the server (from the JSON-RPC spec)
internalError :: JSONRPCException
internalError =
  JSONRPCException { errorCode = -32603
                   , message   = "Internal error"
                   , errorData = Nothing
                   , errorID   = Nothing
                   , errorStdOut = Nothing
                   , errorStdErr = Nothing
                   }

-- | The provided State ID is not one that the server has previously sent.
unknownStateID :: StateID -> JSONRPCException
unknownStateID sid =
  JSONRPCException { errorCode = 20
                   , message = "Unknown state ID"
                   , errorData = Just $ JSON.toJSON sid
                   , errorID = Nothing
                   , errorStdOut = Nothing
                   , errorStdErr = Nothing
                   }

-- | The provided State ID is not one that the server has previously sent.
unexpectedStateID :: JSONRPCException
unexpectedStateID =
  JSONRPCException { errorCode = 21
                   , message = "Unexpected state ID"
                   , errorData = Nothing
                   , errorID = Nothing
                   , errorStdOut = Nothing
                   , errorStdErr = Nothing
                   }

-- | The server is already at capacity and cannot allocate a new state.
serverAtCapacity :: JSONRPCException
serverAtCapacity =
  JSONRPCException { errorCode = 22
                   , message   = "Server at max capacity"
                   , errorData = Nothing
                   , errorID   = Nothing
                   , errorStdOut = Nothing
                   , errorStdErr = Nothing
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
    , errorStdOut = Nothing
    , errorStdErr = Nothing
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
  Request { requestMethod :: !Text
          -- ^ The method name to invoke
          , requestID :: !(Maybe RequestID)
          -- ^ Because the presence of null and the absence of the key are
          -- distinct, we have both a null constructor and wrap it in a Maybe.
          -- When the request ID is not present, the request is a notification
          -- and the field is Nothing.
          , requestParams :: !JSON.Object
          -- ^ The parameters to the method, if any exist
          }
  deriving (Show)


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
      , "method"  .= requestMethod req
      , "id"      .= requestID     req
      , "params"  .= requestParams req ]
  toEncoding req =
    JSON.pairs $
      "jsonrpc" .= jsonRPCVersion         <>
      "method"  .= requestMethod req <>
      "id"      .= requestID     req <>
      "params"  .= requestParams req

-- | A response to a command or query.
--
-- The first type parameter represents the type of state IDs used at
-- the particular stage of the program. When a response is first
-- constructed, the state ID does not yet exist, so the type parameter
-- is (). It becomes 'StateID' later, once the protocol expects this
-- to exist, and the lack of a generalized 'ToJSON' instance prevents
-- us from forgetting the ID when we encode it.
data Response stateID a
  = Response { responseID :: !RequestID
             , responseAnswer :: !a
             , responseStdOut :: !(Maybe String)
             , responseStdErr :: !(Maybe String)
             , responseStateID :: !stateID
             } deriving (Eq, Ord, Show)

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
      , "id"      .= responseID resp
      , "result"  .= JSON.object
        [ "answer" .= responseAnswer resp
        , "stdout" .= responseStdOut resp
        , "stderr" .= responseStdErr resp
        , "state"  .= responseStateID resp
        ]
      ]

-- | A wrapper around `hCapture` with a type signature that allows for no
-- result, to allow easy toggling between silencing and capturing.
hCaptureWrap :: [Handle] -> IO a -> IO (Maybe String, a)
hCaptureWrap hs a =
  do (out, r) <- hCapture hs a
     return (Just out, r)

-- | A version of `hSilence` with the same type signature as
-- `hCaptureWrap`, to allow easy toggling between silencing and
-- capturing.
hNoCapture :: [Handle] -> IO a -> IO (Maybe String, a)
hNoCapture hs a = (Nothing,) <$> hSilence hs a

methodCapture :: MethodOptions -> [Handle] -> IO a -> IO (Maybe String, a)
methodCapture opts
   | optFileSystemMode opts == ReadOnly = hNoCapture
   | otherwise = hCaptureWrap

tryOutErr ::
  (forall a. [Handle] -> IO a -> IO (Maybe String, a)) ->
  RequestID ->
  IO (s, a) ->
  IO (s, Response () a)
tryOutErr capturer reqID action = do
  (err, (out, res)) <-
     capturer [stderr] . capturer [stdout] $
     try @SomeException (try @JSONRPCException action)
  tryPutOutErr out err
  case res of
    Left e -> let message = Just (JSON.String (T.pack (displayException e)))
                in throwIO $ internalError { errorData = message }
    Right (Left e) -> throwIO $ e { errorStdOut = out, errorStdErr = err }
    Right (Right (st, answer)) -> pure (st, Response reqID answer out err ())
  where
    tryPutOutErr :: Maybe String -> Maybe String -> IO ()
    tryPutOutErr out err = do
      void $ try @IOException (traverse (hPutStr stdout) out)
      void $ try @IOException (traverse (hPutStr stderr) err)

execCommand ::
  Command state result ->
  RequestID ->
  CommandContext ->
  state ->
  IO (state, Response () result)
execCommand c reqID ctx st =
  tryOutErr (methodCapture (cmdCtxMOptions ctx)) reqID (runCommand c ctx st)

execQuery ::
  Query state result ->
  RequestID ->
  QueryContext ->
  state ->
  IO (Response () result)
execQuery q reqID ctx st =
  snd <$> tryOutErr (methodCapture (queryCtxMOptions ctx)) reqID (do res <- runQuery q ctx st; pure (st, res))

execNotification ::
  Notification () ->
  NotificationContext ->
  IO ()
execNotification n nctx = do
  void $ tryOutErr hNoCapture IDNull (do runNotification n nctx; pure ((),()))


handleRequest ::
  forall s.
  HasCallStack =>
  MethodOptions ->
  (BS.ByteString -> IO ()) ->
  App s ->
  Request ->
  IO ()
handleRequest opts respond app req =
  case M.lookup method $ appMethods app of
    Nothing -> throwIO $ (methodNotFound method) { errorID = reqID }
    Just (CommandMethod m) -> withRequestID $ \reqID -> do
      stateID <- getStateID req
      response <- withMVar theState $ \state -> do
        when (stateID == initialStateID) $ do
          stateCount <- statePoolCount state
          when (stateCount >= (optMaxOccupancy opts)) $
            throwIO serverAtCapacity
        getAppState state stateID >>=
          \case
            Nothing -> throwIO $ unknownStateID stateID
            Just appState -> do
              let cctx = CommandContext
                          { cmdCtxMOptions = opts
                          , cmdCtxFileReader = B.readFile
                          }
              let cmd = commandImplementation m
              (newAppState, result) <- execCommand (cmd params) reqID cctx appState
              sid' <- nextAppState state stateID newAppState
              return $ addStateID sid' result
      respond (JSON.encode response)
    Just (QueryMethod m) -> withRequestID $ \reqID -> do
      stateID <- getStateID req
      response <- withMVar theState $ \state -> do
        getAppState state stateID >>=
          \case
            Nothing -> throwIO $ unknownStateID stateID
            Just appState -> do
              let qctx = QueryContext
                          { queryCtxMOptions = opts
                          , queryCtxFileReader = B.readFile
                          }
              let q = queryImplementation m
              result <- execQuery (q params) reqID qctx appState
              return $ addStateID stateID result
      respond (JSON.encode response)
    Just (NotificationMethod m) ->
      withoutRequestID $ withoutStateID $ do
        withMVar theState $ \state ->
          let nctx = NotificationContext
                      { ntfCtxMOptions = opts
                      , ntfCtxFileReader = B.readFile
                      , ntfCtxDestroyState = destroyAppState state
                      , ntfCtxDestroyAllStates = destroyAllAppStates state
                      }
              notify = notificationImplementation m
          in execNotification (notify params) nctx
  where
    method   = requestMethod req
    params   = JSON.Object $ requestParams req
    reqID    = requestID req
    theState = serverState app

    addStateID :: StateID -> Response anyOldStateID a -> Response StateID a
    addStateID sid answer = answer {responseStateID = sid}

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

    withoutStateID :: IO a -> IO a
    withoutStateID act =
      if HM.member "state" (requestParams req)
      then throwIO unexpectedStateID
      else act


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
serveStdIO :: HasCallStack => MethodOptions -> App s -> IO ()
serveStdIO opts = serveHandles opts stdin stdout

getStateID :: Request -> IO StateID
getStateID req =
  case HM.lookup "state" (requestParams req) of
    Just sid ->
      case JSON.fromJSON sid of
        JSON.Success i -> pure i
        JSON.Error msg -> noStateID msg
    Nothing -> noStateID "No state field in parameters"
  where
    noStateID msg =
      throwIO $
        invalidParams msg $ JSON.Object $ requestParams req

-- | Serve an application, listening for input on one handle and
-- sending output to another. Each request must be on a line for
-- itself, and no newlines are otherwise allowed.
serveHandles ::
  HasCallStack =>
  MethodOptions {- ^ options for how methods should execute -} ->
  Handle {- ^ input handle    -} ->
  Handle {- ^ output handle   -} ->
  App s  {- ^ RPC application -} ->
  IO ()
serveHandles opts hIn hOut app = init >>= loop
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
                  Right req -> handleRequest opts out app req)
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

-- | Serve an application on stdio, with messages encoded as netstrings.
serveStdIONS :: HasCallStack => MethodOptions -> App s -> IO ()
serveStdIONS opts = serveHandlesNS opts stdin stdout

-- | Serve an application on arbitrary handles, with messages
-- encoded as netstrings.
serveHandlesNS ::
  HasCallStack =>
  MethodOptions   {- ^ options for how methods should execute -} ->
  Handle          {- ^ input handle     -} ->
  Handle          {- ^ output handle    -} ->
  App s           {- ^ RPC application  -} ->
  IO ()
serveHandlesNS opts hIn hOut app =
  do hSetBinaryMode hIn True
     hSetBuffering hIn NoBuffering
     input <- newMVar hIn
     output <- synchronized (\msg ->
                               do optLogger opts (T.pack (show msg))
                                  BS.hPut hOut $ encodeNetstring $ netstring msg
                                  hFlush hOut)
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
      do optLogger opts (T.pack (show line))
         forkIO $
           case JSON.eitherDecode (decodeNetstring line) of
             Left  msg -> throwIO (parseError (T.pack msg))
             Right req -> handleRequest opts output app req
           `catch` reportError output
           `catch` reportOtherException output

    reportError :: (BS.ByteString -> IO ()) -> JSONRPCException -> IO ()
    reportError output exn =
      output (JSON.encode exn)

    reportOtherException :: (BS.ByteString -> IO ()) -> SomeException -> IO ()
    reportOtherException out exn =
      out $ JSON.encode $
      internalError { errorData = Just (JSON.String (T.pack (show exn)))
                    , errorStdOut = Nothing
                    , errorStdErr = Nothing
                    }


-- | Environment variable which, when set to a non-empty value not equal to "0",
-- enables TLS connections over HTTPS.
tlsEnvVar :: String
tlsEnvVar = "TLS_ENABLE"

-- | HTTP-specific options.
data HttpOptions =
  HttpOptions
  { -- | Path at which the API is served.
    httpServerPath :: String,
    -- | Whether or not to use TLS/HTTPS.
    httpUseTLS :: Bool
  }

-- | Serve the application over HTTP, according to the specification
-- at https://www.simple-is-better.org/json-rpc/transport_http.html
serveHttp ::
  HasCallStack =>
  MethodOptions     {- ^ options for how methods should execute -} ->
  HttpOptions            {- ^ Request path -} ->
  App s             {- ^ JSON-RPC app -} ->
  Int               {- ^ port number  -} ->
  IO ()
serveHttp opts httpOpts app port = do
    tls_enable <- lookupEnv tlsEnvVar
    let http_mode = let tlsSetup = scottyTLS port "server.key" "server.crt"
                    in case tls_enable of
                         Just val | val `notElem` ["0", ""] -> tlsSetup
                         _ -> if (httpUseTLS httpOpts) then tlsSetup else scotty port
    http_mode $
      do post (literal (httpServerPath httpOpts)) $
           do ensureTextJSON "Content-Type" unsupportedMediaType415
              ensureTextJSON "Accept" badRequest400
              validateLength
              b <- body
              replyBodyContents <- liftIO $ newMVar mempty
              let respond = \str -> modifyMVar replyBodyContents $ \old -> pure (old <> str, ())
              res <-
                liftIO $
                  (case JSON.eitherDecode b of
                    Left msg ->
                      throwIO (parseError (T.pack msg))
                    Right req ->
                      Right <$> handleRequest opts respond app req)
                      `catch` (\ (exn :: JSONRPCException) ->
                                 pure $ Left $ JSON.encode exn)
                      `catch` (\ (exn :: SomeException) ->
                                 pure $ Left $ JSON.encode (internalError { errorData = Just (JSON.String (T.pack (show exn))) }))
              case res of
                Left err ->
                  do setHeader "Content-Type" "application/json"
                     status badRequest400
                     raw $ err <> BS.singleton newline
                Right () ->
                  do setHeader "Content-Type" "application/json"
                     body <- liftIO (readMVar replyBodyContents)
                     status $ if body == ""
                              then status204
                              else ok200
                     raw body
         -- Return a more informative status code when the wrong HTTP method is used
         for_ [GET, HEAD, PUT, DELETE, TRACE, CONNECT, OPTIONS] $
           \method ->
             addroute method (literal (httpServerPath httpOpts)) $
             do status methodNotAllowed405
                text "Please use POST requests to access the API."

  where
    newline = 0x0a -- ASCII/UTF8
    validateLength =
      do len <- header "Content-Length"
         case len >>= (readMaybe . TL.unpack) of
           Nothing -> do status lengthRequired411
                         text "Missing or invalid \"Content-Length\" header."
                         finish
           Just l ->
             do b <- body
                if BS.length b == l
                  then pure ()
                  else do status badRequest400
                          text "Mismatched length"
                          finish
    ensureTextJSON h stat =
      do contentType <- header h
         if contentType == Just "application/json"
           then pure ()
           else do status stat
                   text $ "The header \"" <> h <> "\" should be \"application/json\"."
                   finish
