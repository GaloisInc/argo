{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SAWServer.CryptolSetup where

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
import SAWServer.OK


startCryptolSetup :: StartCryptolSetupParams -> Method SAWState OK
startCryptolSetup (StartCryptolSetupParams n) =
  do sc <- view sawSC <$> getState
     cenv <- liftIO $ CEnv.initCryptolEnv sc
     pushTask (CryptolSetup n cenv)
     ok

data StartCryptolSetupParams =
  StartCryptolSetupParams { name :: ServerName }

instance FromJSON StartCryptolSetupParams where
  parseJSON =
    withObject "params for \"SAW/cryptol setup\"" $ \o ->
    StartCryptolSetupParams <$> o .: "name"

cryptolSetupLoadModule :: CryptolSetupLoadModuleParams -> Method SAWState OK
cryptolSetupLoadModule (CryptolSetupLoadModuleParams modName) =
  cryptolSetupMethod $
    \cenv ->
      do sc <- view sawSC <$> getState
         let qual = Nothing -- TODO add field to params
         let importSpec = Nothing -- TODO add field to params
         cenv' <- liftIO $ CEnv.importModule sc cenv (Right modName) qual importSpec
         return (cenv', OK)


data CryptolSetupLoadModuleParams =
  CryptolSetupLoadModuleParams P.ModName

instance FromJSON CryptolSetupLoadModuleParams where
  parseJSON =
    withObject "params for \"SAW/Cryptol setup/load module\"" $ \o ->
    CryptolSetupLoadModuleParams . textToModName <$> o .: "module name"

cryptolSetupLoadFile :: CryptolSetupLoadFileParams -> Method SAWState OK
cryptolSetupLoadFile (CryptolSetupLoadFileParams fileName) =
  cryptolSetupMethod $
    \cenv ->
      do sc <- view sawSC <$> getState
         let qual = Nothing -- TODO add field to params
         let importSpec = Nothing -- TODO add field to params
         cenv' <- liftIO $ CEnv.importModule sc cenv (Left fileName) qual importSpec
         return (cenv', OK)


data CryptolSetupLoadFileParams =
  CryptolSetupLoadFileParams FilePath

instance FromJSON CryptolSetupLoadFileParams where
  parseJSON =
    withObject "params for \"SAW/Cryptol setup/load module\"" $ \o ->
    CryptolSetupLoadFileParams . T.unpack <$> o .: "file name"



cryptolSetupMethod :: (CryptolEnv -> Method SAWState (CryptolEnv, a)) -> Method SAWState a
cryptolSetupMethod act =
  do tasks <- view sawTask <$> getState
     case tasks of
       ((CryptolSetup n cenv, env) : moreTasks) ->
         do (cenv', x) <- act cenv
            modifyState (set sawTask $ (CryptolSetup n cenv', env) : moreTasks)
            return x
       _ -> raise notSettingUpCryptol
