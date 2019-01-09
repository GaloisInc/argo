> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GADTSyntax #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE TypeApplications #-}
> module JSONRPC where

> import Control.Applicative
> import Control.Concurrent
> import Control.Concurrent.MVar
> import Control.Exception
> import Control.Lens hiding ((.=))
> import qualified Data.Aeson as JSON
> import Data.Aeson ((.:), (.:!), (.=))
> import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)
> import qualified Data.ByteString.Lazy as BS
> import Data.Map (Map)
> import qualified Data.Map as M
> import Data.Maybe (maybeToList)
> import Data.Scientific (Scientific)
> import Data.Text (Text)
> import qualified Data.Text as T
> import System.IO



A server can receive requests or notifications, and must reply to them.

A server has _state_, which is stored in an MVar to allow easy
concurrency, a collection of methods, each of which is a function
from the JSON value that is the parameter to a JSON value that is the
response.

Methods come in three forms:
 - commands, which can modify state and return an answer to the client;
 - queries, which return an answer but do not modify state; and
 - notifications, which can modify state but do not return an answer.

> data Method s where
>   Command      :: (s -> JSON.Value -> IO (s, JSON.Value)) -> Method s
>   Query        :: (s -> JSON.Value -> IO JSON.Value)      -> Method s
>   Notification :: (s -> JSON.Value -> IO s)               -> Method s

An application is a state and a mapping from names to methods.

> data App s =
>   App { _appState :: MVar s
>       , _appMethods :: Map Text (Method s)
>       }
>
> appState :: Simple Lens (App s) (MVar s)
> appState = lens _appState (\a s -> a { _appState = s })
>
> appMethods :: Simple Lens (App s) (Map Text (Method s))
> appMethods = lens _appMethods (\a s -> a { _appMethods = s })

> mkApp :: s -> [(Text, Method s)] -> IO (App s)
> mkApp initState methods =
>   App <$> newMVar initState <*> pure (M.fromList methods)

JSON RPC exceptions should be thrown by method implementations when
they want to return an error.

> data JSONRPCException =
>   JSONRPCException { errorCode :: Integer
>                    , message :: Text
>                    , errorData :: Maybe JSON.Value
>                    , errorID :: Maybe RequestID
>                    }
>   deriving Show

> instance Exception JSONRPCException

> instance JSON.ToJSON JSONRPCException where
>   toJSON exn =
>     JSON.object
>       [ "jsonrpc" .= the @Text "2.0"
>       , "id" .=
>         case errorID exn of
>           Nothing -> JSON.Null
>           Just theID -> JSON.toJSON theID
>       , "error" .=
>         (JSON.object $ ["code" .= JSON.Number (fromInteger (errorCode exn))
>                        , "message" .= JSON.String (message exn)] ++
>                        (("data" .=) <$> maybeToList (errorData exn)))
>       ]


The spec defines some errors and reserves some error codes (from
-32768 to -32000) for its own use.

> parseError :: Text -> JSONRPCException
> parseError msg =
>   JSONRPCException { errorCode = -32700
>                    , message   = "Parse error"
>                    , errorData = Just (JSON.String msg)
>                    , errorID   = Nothing
>                    }

> methodNotFound :: Maybe RequestID -> JSONRPCException
> methodNotFound theID =
>   JSONRPCException { errorCode = -32601
>                    , message   = "Method not found"
>                    , errorData = Nothing
>                    , errorID   = theID
>                    }
> invalidRequest :: JSONRPCException
> invalidRequest =
>   JSONRPCException { errorCode = -32600
>                    , message   = "Invalid Request"
>                    , errorData = Nothing
>                    , errorID   = Nothing
>                    }


A JSON-RPC request ID is:

    An identifier established by the Client that MUST contain a
    String, Number, or NULL value if included. If it is not included
    it is assumed to be a notification. The value SHOULD normally not
    be Null [1] and Numbers SHOULD NOT contain fractional parts

> data RequestID = IDText !Text | IDNum !Scientific | IDNull
>   deriving (Eq, Ord, Show)

> instance JSON.FromJSON RequestID where
>   parseJSON (JSON.String str) = pure (IDText str)
>   parseJSON (JSON.Number i)   = pure (IDNum i)
>   parseJSON JSON.Null         = pure IDNull
>   parseJSON other             = JSON.typeMismatch "Request ID" other
>
> instance JSON.ToJSON RequestID where
>   toJSON (IDText str) = JSON.String str
>   toJSON (IDNum i)    = JSON.Number i
>   toJSON IDNull       = JSON.Null


Because the presence of null and the absence of the key are distinct,
we have both a null constructor and wrap it in a Maybe. When the
request ID is not present, the request is a notification and the field
is Nothing.

> data Request =
>   Request { _requestMethod :: !Text
>           , _requestID :: !(Maybe RequestID)
>           , _requestParams :: !JSON.Value
>           }

> requestMethod :: Simple Lens Request Text
> requestMethod = lens _requestMethod (\r m -> r { _requestMethod = m })

> requestID :: Simple Lens Request (Maybe RequestID)
> requestID = lens _requestID (\r i -> r { _requestID = i })

> requestParams :: Simple Lens Request JSON.Value
> requestParams = lens _requestParams (\r m -> r { _requestParams = m })

> suchThat :: JSON.Parser a -> (a -> Bool) -> JSON.Parser a
> suchThat parser pred =
>   do res <- parser
>      if pred res
>        then return res
>        else fail "invalid value"

> instance JSON.FromJSON Request where
>   parseJSON =
>     JSON.withObject "JSON-RPC 2.0 request" $
>     \o ->
>       (o .: "jsonrpc" `suchThat` rightVersion) *>
>       (Request <$> o .: "method" <*> o .:! "id" <*> o .: "params")
>     where
>       rightVersion :: Text -> Bool
>       rightVersion v = v == "2.0"


> handleRequest :: forall s . MVar Handle -> App s -> Request -> IO ()
> handleRequest outH app req =
>   let method   = view requestMethod req
>       params   = view requestParams req
>       reqID    = view requestID req
>       theState = view appState app
>   in
>     case M.lookup method $ view appMethods app of
>       Nothing -> throw $ methodNotFound reqID
>       Just m ->
>         requireID m reqID *>
>         case m of
>           Command impl ->
>             do answer <- modifyMVar theState $ flip impl params
>                let response = JSON.object [ "jsonrpc" .= JSON.String "2.0"
>                                           , "id" .= reqID
>                                           , "result" .= answer
>                                           ]
>                withMVar outH $ \h -> BS.hPut h $ JSON.encode response
>           Query impl ->
>             do answer <- withMVar theState $ flip impl params
>                let response = JSON.object [ "jsonrpc" .= JSON.String "2.0"
>                                           , "id" .= reqID
>                                           , "result" .= answer
>                                           ]
>                withMVar outH $ \h -> BS.hPut h $ JSON.encode response
>           Notification impl ->
>                modifyMVar theState $ fmap (,()) . flip impl params
>
>   where
>     requireID :: Method s -> Maybe RequestID -> IO ()
>     requireID (Command _)      (Just _) = return ()
>     requireID (Query _)        (Just _) = return ()
>     requireID (Notification _) Nothing  = return ()
>     requireID _                _        = throw invalidRequest

One way to run a server is on stdio, listening for requests on stdin
and replying on stdout. In this system, each request must be on a
line for itself, and no newlines are otherwise allowed.

> serveStdIO :: App s -> IO ()
> serveStdIO app = init >>= loop
>   where
>     newline = 0x0a -- ASCII/UTF8
>     init = (,) <$> newMVar stdout <*> (BS.split newline <$> BS.hGetContents stdin)
>     loop (output, input) =
>       case input of
>         [] -> return ()
>         (l:rest) ->
>           do forkIO $
>                (case JSON.eitherDecode l of
>                   Left msg -> throw (parseError (T.pack msg))
>                   Right req -> handleRequest output app req)
>                `catch` reportError output
>              loop (output, rest)
>     reportError :: MVar Handle -> JSONRPCException -> IO ()
>     reportError output exn =
>       withMVar output $ \h -> BS.hPut h $ JSON.encode exn

Another way is on a socket.

>


Finally, HTTP also works.

>

Here begin the miscellaneous helpers.

> the :: forall a . a -> a
> the x = x
