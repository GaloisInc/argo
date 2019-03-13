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

type HistoryCommand s = s -> Value -> IO s

historyWrapper ::
  (s -> IO Bool)     {- ^ validate      -} ->
  [(Text, MethodType, Value -> Method s Value)] {- ^ all methods   -} ->
  [(Text, MethodType, Value -> Method (HistoryWrapper s) Value)]
historyWrapper validate methods =
  [ (name, Query, wrapMethod commands validate name t m)
  | (name, t, m) <- methods ]
  where
    commands = methodsToCommands methods

-- | Extract the commands from a list of supported methods. These commands
-- are the ones we'll need to remember because they can update the state.
methodsToCommands ::
  [(Text, MethodType, Value -> Method s Value)]  {- ^ all methods   -} ->
  [(Text, HistoryCommand s)]                     {- ^ commands only -}
methodsToCommands methods =
  [ (name, command)
  | (name, ty, method) <- methods
  , Just command <- [extractCommand ty method]
  ]

-- | Extract the components of methods that affect the server's state.
extractCommand :: MethodType -> (Value -> Method s Value) -> Maybe (HistoryCommand s)
extractCommand Query _ = Nothing
extractCommand _     m = Just $ \s p -> fst <$> runMethod (m p) s

------------------------------------------------------------------------

-- | Wrap a JSON RPC method to use explicit state representation.
--
-- Commands are wrapped as commands. The result is paired up with the
-- new steps list via 'injectSteps'
--
-- Queries are wrapped as commands. The result is passed through directly.
-- Because queries do not alter the state, we don't return a new list
-- of state steps. The result is simply passed through.
--
-- Notifications are perhaps surprisingly wrapped as commands, too. The
-- new list of steps is returned as the result directly.
wrapMethod ::
  [(Text, HistoryCommand s)]   {- ^ commands              -} ->
  (s -> IO Bool)               {- ^ validate              -} ->
  Text                         {- ^ method name           -} ->
  MethodType                   {- ^ what kind of method is it -} ->
  (Value -> Method s Value)    {- ^ method implementation -} ->
  (Value -> Method (HistoryWrapper s) Value)

wrapMethod commands validate name Query q =
  \params ->
  do (steps, params') <- extractStepsM params
     hs               <- getState
     cache            <- liftIO $ cacheLookup (runHistoryCommand commands) validate (historyCache hs) steps
     (_, result)      <- liftIO $ runMethod (q params') (cacheRoot cache)
     return $ Object (HashMap.fromList [("answer", result)])

wrapMethod commands validate name methodType c =
  \params ->
  do (steps, params') <- extractStepsM params
     hs               <- getState
     let cmd           = (name, params')
     cache            <- liftIO $ cacheLookup (runHistoryCommand commands) validate (historyCache hs) steps
     (s', result)     <- liftIO $ runMethod (c params') (cacheRoot cache)
     _                <- liftIO $ cacheAdvance (\_ _ -> return s') (\_ -> return True) cache cmd
     let steps'        = steps ++ [cmd]
     return . injectSteps steps' $
       case methodType of
         Command -> result
         Notification -> object []
         Query -> error "Internal error: impossible pattern match"

------------------------------------------------------------------------

stateKey :: Text
stateKey = "state"

answerKey :: Text
answerKey = "answer"

extractStepsM :: Monad m => Value -> m ([(Text, Value)], Value)
extractStepsM v =
  case extractSteps v of
    Nothing -> fail "Missing state parameter"
    Just x -> return x

extractSteps :: Value -> Maybe ([(Text, Value)], Value)
extractSteps v
  | Object o <- v
  , Just history <- HashMap.lookup stateKey o
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
  [(Text, HistoryCommand s)] {- ^ command handlers -} ->
  (Text, Value)              {- ^ step sequence    -} ->
  s                          {- ^ initial state    -} ->
  IO s                       {- ^ final state      -}
runHistoryCommand commands (name, params) s =
  case lookup name commands of
    Nothing   -> fail ("Unknown command: " ++ show name)
    Just impl -> impl s params
