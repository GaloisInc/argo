{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SAWServer.CryptolSetup
  ( startCryptolSetup
  , cryptolSetupLoadFile
  , cryptolSetupLoadModule
  , cryptolSetupDone
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class
import Control.Lens
import Data.Aeson (FromJSON(..), withObject, (.:))
import qualified Data.Text as T

import qualified Cryptol.Parser.AST as P
import Cryptol.Utils.Ident (textToModName)
import SAWScript.Value (biSharedContext)
import qualified Verifier.SAW.CryptolEnv as CEnv
import Verifier.SAW.CryptolEnv (CryptolEnv)

import Argo
import SAWServer
import SAWServer.Exceptions
import SAWServer.NoParams
import SAWServer.OK


startCryptolSetup :: StartCryptolSetupParams -> Method SAWState OK
startCryptolSetup (StartCryptolSetupParams n) =
  do sc <- biSharedContext . view sawBIC <$> getState
     cenv <- liftIO $ CEnv.initCryptolEnv sc
     pushTask (CryptolSetup n cenv)
     ok

newtype StartCryptolSetupParams
  = StartCryptolSetupParams ServerName

instance FromJSON StartCryptolSetupParams where
  parseJSON =
    withObject "params for \"SAW/cryptol setup\"" $ \o ->
    StartCryptolSetupParams <$> o .: "name"


cryptolSetupLoadModule :: CryptolSetupLoadModuleParams -> Method SAWState OK
cryptolSetupLoadModule (CryptolSetupLoadModuleParams modName) =
  cryptolSetupMethod $
    \cenv ->
      do sc <- biSharedContext . view sawBIC <$> getState
         let qual = Nothing -- TODO add field to params
         let importSpec = Nothing -- TODO add field to params
         cenv' <- liftIO $ CEnv.importModule sc cenv (Right modName) qual importSpec
         debugLog "loaded"
         return (cenv', OK)

newtype CryptolSetupLoadModuleParams
  = CryptolSetupLoadModuleParams P.ModName

instance FromJSON CryptolSetupLoadModuleParams where
  parseJSON =
    withObject "params for \"SAW/Cryptol setup/load module\"" $ \o ->
    CryptolSetupLoadModuleParams . textToModName <$> o .: "module name"


cryptolSetupLoadFile :: CryptolSetupLoadFileParams -> Method SAWState OK
cryptolSetupLoadFile (CryptolSetupLoadFileParams fileName) =
  cryptolSetupMethod $
    \cenv ->
      do sc <- biSharedContext . view sawBIC <$> getState
         let qual = Nothing -- TODO add field to params
         let importSpec = Nothing -- TODO add field to params
         cenv' <- liftIO $ try $ CEnv.importModule sc cenv (Left fileName) qual importSpec
         case cenv' of
           Left (ex :: SomeException) -> raise $ cryptolError (T.pack (show ex))
           Right cenv'' -> return (cenv'', OK)

newtype CryptolSetupLoadFileParams
  = CryptolSetupLoadFileParams FilePath

instance FromJSON CryptolSetupLoadFileParams where
  parseJSON =
    withObject "params for \"SAW/Cryptol setup/load file\"" $ \o ->
    CryptolSetupLoadFileParams . T.unpack <$> o .: "file"


cryptolSetupDone :: NoParams -> Method SAWState OK
cryptolSetupDone NoParams =
  do tasks <- view sawTask <$> getState
     case tasks of
       ((CryptolSetup n cenv, _) : _) ->
         do dropTask
            setServerVal n cenv
            ok
       _ -> raise notSettingUpCryptol


cryptolSetupMethod :: (CryptolEnv -> Method SAWState (CryptolEnv, a)) -> Method SAWState a
cryptolSetupMethod act =
  do tasks <- view sawTask <$> getState
     case tasks of
       ((CryptolSetup n cenv, env) : moreTasks) ->
         do (cenv', x) <- act cenv
            modifyState (set sawTask $ (CryptolSetup n cenv', env) : moreTasks)
            return x
       _ -> raise notSettingUpCryptol
