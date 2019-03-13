{-# Language OverloadedStrings #-}
module Argo.HistoryWrapper
  ( HistoryWrapper(..)
  , historyWrapper
  ) where

import Argo.JSONRPC
import Argo.CacheTree

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Data.Text (Text)
import Data.Aeson (Result(..), Value(..), fromJSON, toJSON, object)
import qualified Data.HashMap.Strict as HashMap

data HistoryWrapper s = HistoryWrapper
  { historyCache :: Cache s (Text, Value) }

historyWrapper ::
  (s -> IO Bool) {- ^ validate state -} ->
  [(Text, MethodType, Value -> Method                 s  Value)]
  {- ^ all methods   -} ->
  [(Text, MethodType, Value -> Method (HistoryWrapper s) Value)]
historyWrapper validate methods =
  [ (name, Query, wrapMethod commands validate name t m)
  | (name, t, m) <- methods ]
  where
    commands = methodsToCommands methods

-- | Extract the stateful commands from a list of supported methods. These
-- commands are the ones we'll need to remember because they can update the
-- state.
methodsToCommands ::
  [(Text, MethodType, Value -> Method s Value)]  {- ^ all methods   -} ->
  [(Text, s -> Value -> IO s)]                   {- ^ don't include 'Query' -}
methodsToCommands methods =
  [ (name, command)
  | (name, ty, method) <- methods
  , Just command <- [extractCommand ty method]
  ]

-- | Extract the components of methods that affect the server's state.
extractCommand ::
  MethodType ->
  (Value -> Method s Value) ->
  Maybe (s -> Value -> IO s)
extractCommand Query _ = Nothing
extractCommand _     m = Just $ \s p -> fst <$> runMethod (m p) s

------------------------------------------------------------------------

-- | Wrap a JSON RPC method to use explicit state representation.
--
-- A stateful 'Command' type 'Method' can be made into a stateless 'Query' type
-- 'Method' by pairing it with its full history, making use of caching to avoid
-- repeating work where possible.
wrapMethod ::
  [(Text, s -> Value -> IO s)]   {- ^ all methods               -} ->
  (s -> IO Bool)                 {- ^ is state still valid?     -} ->
  Text                           {- ^ method name               -} ->
  MethodType                     {- ^ what kind of method is it -} ->
  (Value -> Method s Value)      {- ^ method implementation     -} ->
  (Value -> Method (HistoryWrapper s) Value)

wrapMethod commands validate name Query q =
  withState $ \hs steps params ->
  do cache            <- cacheLookup
                           (runHistoryCommand commands)
                           validate
                           (historyCache hs)
                           steps
     (_, result)      <- runMethod (q params) (cacheRoot cache)
     return $ Object (HashMap.fromList [("answer", result)])

wrapMethod commands validate name methodType c =
  withState $ \hs steps params ->
  do let cmd           = (name, params)
     cache            <- cacheLookup
                           (runHistoryCommand commands)
                           validate
                           (historyCache hs)
                           steps
     (s', result)     <- runMethod (c params) (cacheRoot cache)
     _                <- cacheAdvance
                           (\_ _ -> return s')
                           (\_ -> return True)
                           cache
                           cmd
     let steps'        = steps ++ [cmd]
         output        = case methodType of
                           Command      -> result
                           Notification -> object []
                           Query        -> error "Internal error: impossible pattern match"
     return (injectSteps steps' output)

-- | Captures the common behavior in 'wrapMethod'. Given a continuation
-- using the history state, list of commands used to reach current state,
-- and the raw parameters object compute the method that results a result
-- value which is wrapped with any new steps.
withState ::
  (HistoryWrapper s -> [(Text, Value)] -> Value -> IO Value)
    {- ^ continuation: state, steps, parameters object to result -} ->
  Value {- ^ raw parameters object -} ->
  Method (HistoryWrapper s) Value
withState k params =
  do hs               <- getState
     (steps, params') <- extractStepsM params
     liftIO (k hs steps params')

-- | Extract the state field from a parameter object or raise
-- a JSONRPC error.
extractStepsM ::
  Value                             {- ^ raw parameters object       -} ->
  Method s ([(Text, Value)], Value) {- ^ steps, remaining parameters -}
extractStepsM v =
  case extractSteps v of
    Just x  -> return x
    Nothing -> raise (makeJSONRPCException
                        32000
                        "Missing state field"
                        (Nothing :: Maybe ()))

------------------------------------------------------------------------

stateKey :: Text
stateKey = "state"

answerKey :: Text
answerKey = "answer"

extractSteps :: Value -> Maybe ([(Text, Value)], Value)
extractSteps v
  | Object o      <- v
  , Just history  <- HashMap.lookup stateKey o
  , Success steps <- fromJSON history
  , let v' = Object (HashMap.delete stateKey o) = Just (steps, v')
extractSteps _ = Nothing

-- | Combine a command result and the current sequence of steps
-- together into a single JSON value.
injectSteps ::
  [(Text, Value)] {- ^ command steps  -} ->
  Value           {- ^ command result -} ->
  Value           {- ^ combined value -}
injectSteps steps result =
  Object (HashMap.fromList [(stateKey, toJSON steps), (answerKey, result)])

runHistoryCommand ::
  [(Text, s -> Value -> IO s)] {- ^ command handlers -} ->
  (Text, Value)                {- ^ step sequence    -} ->
  s                            {- ^ initial state    -} ->
  IO s                         {- ^ final state      -}
runHistoryCommand commands (name, params) s =
  case lookup name commands of
    Nothing   -> fail ("Unknown command: " ++ show name)
    Just impl -> impl s params
