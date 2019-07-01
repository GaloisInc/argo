{-# LANGUAGE OverloadedStrings #-}
module SAWServer.CrucibleSetup where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Lens hiding ((.:))
import Data.Aeson (FromJSON(..), withObject, withText, (.:))
import qualified Data.Text as T

import qualified Cryptol.Parser.AST as P
import Cryptol.Utils.Ident (textToModName)
import qualified Verifier.SAW.CryptolEnv as CEnv
import Verifier.SAW.CryptolEnv (CryptolEnv)

import Argo
import SAWServer
import SAWServer.Exceptions
import SAWServer.NoParams
import SAWServer.OK


startCrucibleSetup :: StartCrucibleSetupParams -> Method SAWState OK
startCrucibleSetup (StartCrucibleSetupParams n) =
  do pushTask (LLVMCrucibleSetup n (return ()))
     ok

data StartCrucibleSetupParams =
  StartCrucibleSetupParams { name :: ServerName }

instance FromJSON StartCrucibleSetupParams where
  parseJSON =
    withObject "params for \"SAW/Crucible setup\"" $ \o ->
    StartCrucibleSetupParams <$> o .: "name"
