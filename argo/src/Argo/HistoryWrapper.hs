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
import Data.Aeson (Result(..), Value(..), fromJSON, toJSON)
import qualified Data.HashMap.Strict as HashMap

data HistoryWrapper s =
  HistoryWrapper { historyCache :: Cache s (Text, Value) }

type HistoryCommand s = s -> Value -> IO s

historyWrapper ::
  (s -> IO Bool)     {- ^ validate      -} ->
  [(Text, Method s)] {- ^ all methods   -} ->
  [(Text, Method (HistoryWrapper s))]
historyWrapper validate methods =
  [(name, wrapMethod commands validate name m) | (name, m) <- methods]
  where
    commands = methodsToCommands methods

-- | Extract the commands from a list of supported methods. These commands
-- are the ones we'll need to remember because they can update the state.
methodsToCommands ::
  [(Text, Method s)]  {- ^ all methods   -} ->
  [(Text, HistoryCommand s)] {- ^ commands only -}
methodsToCommands = itoListOf (folded . ifolded <. folding extractCommand)

-- | Extract the components of methods that affect the server's state.
extractCommand :: Method s -> Maybe (HistoryCommand s)
extractCommand (Command      c) = Just $ \s p -> fst <$> runCommand IDNull s p c
extractCommand (Notification n) = Just $ \s p -> fst <$> runNotification s p n
extractCommand (Query        _) = Nothing

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
  Method s                     {- ^ method implementation -} ->
  Method (HistoryWrapper s)

wrapMethod commands validate name (Command command) =
  Query $
  do (steps, params') <- extractStepsIO =<< params
     rId              <- getRequestID
     hs               <- getState
     let cmd           = (name, params')
     c                <- cacheLookup (runHistoryCommand commands) validate (historyCache hs) steps
     (s', result)     <- runCommand rId (cacheRoot c) params' command
     _                <- cacheAdvance (\_ _ -> return s') (\_ -> return True) c cmd
     let steps'        = steps ++ [cmd]
     return (injectSteps steps' result)

wrapMethod commands validate name (Query query) =
  Query $
  do (steps, params') <- extractStepsIO =<< params
     rId              <- getRequestID
     hs               <- getState
     c                <- cacheLookup (runHistoryCommand commands) validate (historyCache hs) steps
     result           <- runQuery rId (cacheRoot c) params' query
     return result

wrapMethod commands validate name (Notification notification) =
  Query $
  do (steps, params') <- extractStepsIO =<< params
     let steps'        = steps ++ [(name, params')]
     hs               <- getState
     _                <- cacheLookup (runHistoryCommand commands) validate (historyCache hs) steps'
     return (toJSON steps')

------------------------------------------------------------------------

stateKey :: Text
stateKey = "state"

extractStepsIO :: MonadIO m => Value -> m ([(Text, Value)], Value)
extractStepsIO v =
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
  Object (HashMap.fromList [(stateKey, toJSON steps), ("answer", result)])

runHistoryCommand ::
  [(Text, HistoryCommand s)] {- ^ command handlers -} ->
  (Text, Value)              {- ^ step sequence    -} ->
  s                          {- ^ starting state   -} ->
  IO s                       {- ^ sequenced state  -}
runHistoryCommand commands (name, params) s =
  case lookup name commands of
    Nothing   -> fail ("Unknown command: " ++ show name)
    Just impl -> impl s params
