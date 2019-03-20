{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.LoadModule (loadModule) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson as JSON
import qualified Data.Text as T
import System.Directory
import Data.Functor

import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, checkExpr, evalExpr, loadModuleByPath, loadModuleByName)

import CryptolServer
import Argo


loadModule :: LoadModuleParams -> Method ServerState ()
loadModule (LoadModuleParams fn) =
  void $ runModuleCmd (loadModuleByPath fn)

data LoadModuleParams =
  LoadModuleParams { loadModuleMod :: FilePath }

instance JSON.FromJSON LoadModuleParams where
  parseJSON =
    JSON.withObject "params for \"load module\"" $
    \o -> LoadModuleParams <$> o .: "file"
