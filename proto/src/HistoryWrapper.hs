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

data HistoryWrapper s = HistoryWrapper

type Command s = s -> Value -> IO s

historyWrapper ::
  [(Text, Method s)] {- ^ all methods   -} ->
  s                  {- ^ initial state -} ->
  [(Text, Method (HistoryWrapper s))]
historyWrapper methods startState =
  [(name, wrapMethod commands startState name m) | (name, m) <- methods]
  where
    commands = methodsToCommands methods

-- | Extract the commands from a list of supported methods. These commands
-- are the ones we'll need to remember because they can update the state.
methodsToCommands ::
  [(Text, Method s)]  {- ^ all methods   -} ->
  [(Text, Command s)] {- ^ commands only -}
methodsToCommands = itoListOf (folded . ifolded <. folding extractCommand)

extractCommand :: Method s -> Maybe (Command s)
extractCommand (Command      f) = Just $ \s p -> fst <$> f IDNull s p
extractCommand (Notification f) = Just $ \s p -> f s p
extractCommand (Query        _) = Nothing

------------------------------------------------------------------------

wrapMethod ::
  [(Text, Command s)]   {- ^ commands              -} ->
  s                     {- ^ initial state         -} ->
  Text                  {- ^ method name           -} ->
  Method s              {- ^ method implementation -} ->
  Method (HistoryWrapper s)

wrapMethod commands startState name (Command f) =
  Command $ \rId hs params ->
  do (steps, params') <- extractStepsIO params
     s                <- runCommands commands steps startState
     (_s', result)    <- f rId s params'
     let result' = injectSteps (steps ++ [(name, params')]) result
     return (hs, result')

wrapMethod commands startState name (Query f) =
  Command $ \rId hs params ->
  do (steps, params') <- extractStepsIO params
     s                <- runCommands commands steps startState
     result           <- f rId s params'
     return (hs, result)

wrapMethod commands startState name (Notification f) =
  Notification $ \hs params ->
  do (steps, params') <- extractStepsIO params
     s                <- runCommands commands steps startState
     _s'              <- f s params'
     return hs

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

injectSteps :: [(Text, Value)] -> Value -> Value
injectSteps steps result =
  Object (HashMap.fromList [(stateKey, toJSON steps), ("answer", result)])


runCommands :: [(Text, Command s)] -> [(Text, Value)] -> s -> IO s
runCommands commands history s0 = foldM go s0 history
  where
    go s (name, params) =
      case lookup name commands of
        Nothing -> fail ("Unknown command: " ++ show name)
        Just impl -> impl s params
