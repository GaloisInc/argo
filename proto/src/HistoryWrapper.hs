{-# Language OverloadedStrings #-}
module HistoryWrapper
  ( HistoryWrapper(..)
  , historyWrapper
  ) where

import JSONRPC
import Control.Monad
import Control.Lens
import Data.Text (Text)
import Data.Aeson (Result(..), Value(..), fromJSON, toJSON)
import qualified Data.HashMap.Strict as HashMap
import CacheTree

data HistoryWrapper s = HistoryWrapper
  { historyCache :: Cache s (Text, Value)
  }

type Command s = s -> Value -> IO s

historyWrapper ::
  [(Text, Method s)] {- ^ all methods   -} ->
  [(Text, Method (HistoryWrapper s))]
historyWrapper methods =
  [(name, wrapMethod commands name m) | (name, m) <- methods]
  where
    commands = methodsToCommands methods

-- | Extract the commands from a list of supported methods. These commands
-- are the ones we'll need to remember because they can update the state.
methodsToCommands ::
  [(Text, Method s)]  {- ^ all methods   -} ->
  [(Text, Command s)] {- ^ commands only -}
methodsToCommands = itoListOf (folded . ifolded <. folding extractCommand)

-- | Extract the components of methods that affect the server's state.
extractCommand :: Method s -> Maybe (Command s)
extractCommand (Command      f) = Just $ \s p -> fst <$> f IDNull s p
extractCommand (Notification f) = Just $ \s p -> f s p
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
  [(Text, Command s)]   {- ^ commands              -} ->
  Text                  {- ^ method name           -} ->
  Method s              {- ^ method implementation -} ->
  Method (HistoryWrapper s)

wrapMethod commands name (Command f) =
  Query $ \rId hs params ->
  do (steps, params') <- extractStepsIO params
     let cmd           = (name, params')
     c                <- cacheLookup (runCommand commands) (historyCache hs) steps
     (s', result)     <- f rId (cacheRoot c) params'
     _                <- cacheAdvance (\_ _ -> return s') c cmd
     let steps'        = steps ++ [cmd]
     return (injectSteps steps' result)

wrapMethod commands name (Query f) =
  Query $ \rId hs params ->
  do (steps, params') <- extractStepsIO params
     c                <- cacheLookup (runCommand commands) (historyCache hs) steps
     result           <- f rId (cacheRoot c) params'
     return result

wrapMethod commands name (Notification f) =
  Query $ \_rId hs params ->
  do (steps, params') <- extractStepsIO params
     let steps'        = steps ++ [(name, params')]
     _                <- cacheLookup (runCommand commands) (historyCache hs) steps'
     return (toJSON steps')

------------------------------------------------------------------------

stateKey :: Text
stateKey = "state"

extractStepsIO :: Value -> IO ([(Text, Value)], Value)
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


runCommand ::
  [(Text, Command s)] {- ^ command handlers -} ->
  (Text, Value)       {- ^ step sequence    -} ->
  s                   {- ^ starting state   -} ->
  IO s                {- ^ sequenced state  -}
runCommand commands (name, params) s =
  case lookup name commands of
    Nothing   -> fail ("Unknown command: " ++ show name)
    Just impl -> impl s params
