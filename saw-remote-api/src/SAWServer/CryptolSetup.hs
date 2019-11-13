{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SAWServer.CryptolSetup
  ( cryptolLoadFile
  , cryptolLoadModule
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class
import Control.Lens
import Data.Aeson (FromJSON(..), withObject, (.:))
import qualified Data.Text as T

import qualified Cryptol.Parser.AST as P
import Cryptol.Utils.Ident (textToModName)
import SAWScript.Value (biSharedContext, TopLevelRW(..))
import qualified Verifier.SAW.CryptolEnv as CEnv
import Cryptol.ModuleSystem (loadModuleByPath, loadModuleByName, ModuleEnv, ModuleCmd)
import qualified Cryptol.ModuleSystem.Base as MB

import Argo
import SAWServer
import SAWServer.Exceptions
import SAWServer.NoParams
import SAWServer.OK
import CryptolServer (runCEnvCmd)
import CryptolServer.LoadModule as CryptolServer

cryptolLoadFile :: CryptolLoadFileParams -> Method SAWState OK
cryptolLoadFile (CryptolLoadFileParams file) =
  runCryptolCmd (loadModuleByPath file) >> ok

cryptolLoadModule :: CryptolLoadModuleParams -> Method SAWState OK
cryptolLoadModule (CryptolLoadModuleParams mod) =
  runCryptolCmd (loadModuleByName mod) >> ok

newtype CryptolLoadModuleParams =
  CryptolLoadModuleParams P.ModName

instance FromJSON CryptolLoadModuleParams where
  parseJSON =
    withObject "params for \"SAW/Cryptol setup/load module\"" $ \o ->
    CryptolLoadModuleParams . textToModName <$> o .: "module name"

newtype CryptolLoadFileParams =
  CryptolLoadFileParams FilePath

instance FromJSON CryptolLoadFileParams where
  parseJSON =
    withObject "params for \"SAW/Cryptol setup/load file\"" $ \o ->
    CryptolLoadFileParams . T.unpack <$> o .: "file"
