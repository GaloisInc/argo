{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.LoadModule (loadModule) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson as JSON
import qualified Data.Text as T
import System.Directory

import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, checkExpr, evalExpr, loadModuleByPath, loadModuleByName)

import CryptolServer
import JSONRPC


loadModule :: CryptolServerCommand JSON.Value
loadModule =
  do LoadModuleParams fn <- params
     x <- runModuleCmd (loadModuleByPath fn)
     return (JSON.toJSON ())

data LoadModuleParams =
  LoadModuleParams { loadModuleMod :: FilePath }

instance JSON.FromJSON LoadModuleParams where
  parseJSON =
    JSON.withObject "params for \"load module\"" $
    \o -> LoadModuleParams <$> o .: "file"
