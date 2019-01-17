{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Lens
import qualified Data.Aeson as JSON
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as JSON
import Data.Text (Text)

import JSONRPC

import Cryptol.Eval.Monad (EvalOpts(..), PPOpts(..))
import Cryptol.Parser (parseModName)
import Cryptol.Parser.AST (ModName)
import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, loadModuleByPath, loadModuleByName)
import Cryptol.ModuleSystem.Env (initialModuleEnv)
import Cryptol.Utils.Logger (quietLogger)


main :: IO ()
main =
  do initSt <- initialState
     theApp <- mkApp initSt cryptolMethods
     serveStdIONS theApp

initialState :: IO ServerState
initialState = ServerState Nothing <$> initialModuleEnv



cryptolMethods :: [(Text, Method ServerState)]
cryptolMethods =
  [("load module", Command loadModule)]
  where
    loadModule s params =
      case JSON.fromJSON params of
        JSON.Error msg -> throw (badParams params)
        JSON.Success (LoadModuleParams fn) ->
          do (s', x) <- runModuleCmd s (loadModuleByPath fn)
             return (s', JSON.toJSON ())

theEvalOpts :: EvalOpts
theEvalOpts = EvalOpts quietLogger (PPOpts False 10 25)

runModuleCmd :: ServerState -> ModuleCmd a -> IO (ServerState, a)
runModuleCmd s cmd =
     do out <- cmd (theEvalOpts, view moduleEnv s)
        case out of
          (Left x, warns) -> error "TODO"
          (Right (x, newEnv), warns) -> return (set moduleEnv newEnv s, x)

data LoadModuleParams =
  LoadModuleParams { loadModuleMod :: FilePath }

instance JSON.FromJSON LoadModuleParams where
  parseJSON =
    JSON.withObject "params for \"load module\"" $
    \o -> LoadModuleParams <$> o .: "file"

class ServerParams a where
  parseParams :: JSON.Value -> JSON.Parser (a, [ServerHistory])

instance ServerParams LoadModuleParams where
  parseParams v = (,) <$> JSON.parseJSON v <*> (JSON.withObject "state" (\o -> o .: "state") v)

data ServerHistory =
  HistLoadModule LoadModuleParams

instance JSON.FromJSON ServerHistory where
  parseJSON =
    JSON.withObject ("history entry") $
    \o -> do (cmd :: Text) <- o .: "command"
             case cmd of
               "load module" -> HistLoadModule <$> (o .: "params")
               _ -> empty

-- TODO add request ID (it should be passed to commands and such, I suppose)
badParams :: JSON.Value -> JSONRPCException
badParams params =
  JSONRPCException { errorCode = 2
                   , message = "Bad params"
                   , errorData = Just params
                   , errorID = Nothing
                   }

data LoadedModule = LoadedModule
  { _loadedName :: Maybe ModName   -- ^ Working on this module.
  , _loadedPath :: FilePath        -- ^ Working on this file.
  }

loadedName :: Simple Lens LoadedModule (Maybe ModName)
loadedName = lens _loadedName (\v n -> v { _loadedName = n })

loadedPath :: Simple Lens LoadedModule FilePath
loadedPath = lens _loadedPath (\v n -> v { _loadedPath = n })


data ServerState =
  ServerState { _loadedModule :: Maybe LoadedModule
              , _moduleEnv :: ModuleEnv
              }

loadedModule :: Simple Lens ServerState (Maybe LoadedModule)
loadedModule = lens _loadedModule (\v n -> v { _loadedModule = n })

moduleEnv :: Simple Lens ServerState ModuleEnv
moduleEnv = lens _moduleEnv (\v n -> v { _moduleEnv = n })
