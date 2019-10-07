{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module SAWServer.LLVMCrucibleSetup where

import Control.Applicative
import Control.Lens hiding ((.:))
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Aeson (FromJSON(..), withObject, withText, (.:))
import Data.Parameterized.Some
import qualified Data.Text as T


import qualified Cryptol.Parser.AST as P
import Cryptol.Utils.Ident (textToModName)
import qualified Data.LLVM.BitCode as LLVM
import SAWScript.Crucible.LLVM.Builtins (crucible_return)
import qualified SAWScript.Crucible.LLVM.CrucibleLLVM as Crucible (translateModule)
import qualified SAWScript.Crucible.LLVM.MethodSpecIR as CMS (LLVMModule(..))
import SAWScript.Options (defaultOptions)
import qualified Verifier.SAW.CryptolEnv as CEnv
import Verifier.SAW.CryptolEnv (CryptolEnv)


import Argo
import SAWServer
import SAWServer.Exceptions
import SAWServer.NoParams
import SAWServer.OK
import SAWServer.SetupValue


startLLVMCrucibleSetup :: StartLLVMCrucibleSetupParams -> Method SAWState OK
startLLVMCrucibleSetup (StartLLVMCrucibleSetupParams n) =
  do pushTask (LLVMCrucibleSetup n (return ()))
     ok

data StartLLVMCrucibleSetupParams =
  StartLLVMCrucibleSetupParams { llvmSetupName :: ServerName }

instance FromJSON StartLLVMCrucibleSetupParams where
  parseJSON =
    withObject "params for \"SAW/Crucible setup\"" $ \o ->
    StartLLVMCrucibleSetupParams <$> o .: "name"


llvmCrucibleSetupDone :: NoParams -> Method SAWState OK
llvmCrucibleSetupDone NoParams =
  do tasks <- view sawTask <$> getState
     case tasks of
       ((LLVMCrucibleSetup n setup, _) : _) ->
         do dropTask
            setServerVal n setup
            ok
       _ -> raise notSettingUpLLVMCrucible

data LLVMCrucibleReturnParams =
  LLVMCrucibleReturnParams { llvmReturnVal :: LLVMSetupVal }

instance FromJSON LLVMCrucibleReturnParams where
  parseJSON =
    withObject "params for \"SAW/LLVM/return\"" $ \o ->
    LLVMCrucibleReturnParams <$> o .: "value"

llvmCrucibleReturn :: LLVMCrucibleReturnParams -> Method SAWState OK
llvmCrucibleReturn v =
  do tasks <- view sawTask <$> getState
     case tasks of
       ((LLVMCrucibleSetup n setup, env) : more) ->
         do bic <- view sawBIC <$> getState
            val <- getSetupVal (llvmReturnVal v)
            modifyState $
              set sawTask ((LLVMCrucibleSetup n (setup *> crucible_return bic defaultOptions val), env) :
                           more)
            ok
       _ -> raise notSettingUpLLVMCrucible


data LLVMLoadModuleParams =
  LLVMLoadModuleParams
    { llvmModuleName :: ServerName
    , llvmModuleFilename :: FilePath
    }

instance FromJSON LLVMLoadModuleParams where
  parseJSON =
    withObject "params for \"SAW/LLVM/load module\"" $ \o ->
    LLVMLoadModuleParams <$> o .: "name" <*> o .: "bitcode file"

llvmLoadModule :: LLVMLoadModuleParams -> Method SAWState OK
llvmLoadModule (LLVMLoadModuleParams serverName fileName) =
  do tasks <- view sawTask <$> getState
     case tasks of
       (_:_) -> raise $ notAtTopLevel $ map fst tasks
       [] ->
         liftIO (LLVM.parseBitCodeFromFile fileName) >>= \case
           Left err -> raise (cantLoadLLVMModule (LLVM.formatError err))
           Right llvmMod ->
             do halloc <- getHandleAlloc
                Some mtrans <- liftIO $ Crucible.translateModule halloc llvmMod
                setServerVal serverName (Some (CMS.LLVMModule fileName llvmMod mtrans))
                ok
