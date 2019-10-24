{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module SAWServer.LLVMCrucibleSetup where

import Control.Applicative
import Control.Lens hiding ((.:))
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.ST
import Data.Aeson (FromJSON(..), withObject, withText, (.:))
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Parameterized.Pair
import Data.Parameterized.Some
import Data.Text (Text)
import qualified Data.Text as T


import qualified Cryptol.Parser.AST as P
import Cryptol.Utils.Ident (textToModName)
import qualified Data.LLVM.BitCode as LLVM
import SAWScript.Crucible.Common.MethodSpec (SetupValue(..), PointsTo)
import SAWScript.Crucible.LLVM.Builtins (crucible_alloc, crucible_execute_func, crucible_fresh_var, crucible_points_to, crucible_return)
import qualified SAWScript.Crucible.LLVM.CrucibleLLVM as Crucible (translateModule)
import qualified SAWScript.Crucible.LLVM.MethodSpecIR as CMS (AllLLVM, LLVMModule(..), anySetupTerm, anySetupNull)
import SAWScript.Options (defaultOptions)
import SAWScript.Value (BuiltinContext, LLVMCrucibleSetupM)
import Text.LLVM.AST (Type)
import qualified Verifier.SAW.CryptolEnv as CEnv
import Verifier.SAW.CryptolEnv (CryptolEnv)
import Verifier.SAW.TypedTerm (TypedTerm)

import Argo
import CryptolServer.Data.Expression
import SAWServer
import SAWServer.Data.LLVMType (llvmType)
import SAWServer.CryptolExpression (getTypedTerm)
import SAWServer.Exceptions
import SAWServer.NoParams
import SAWServer.OK
import SAWServer.SetupValue


startLLVMCrucibleSetup :: StartLLVMCrucibleSetupParams -> Method SAWState OK
startLLVMCrucibleSetup (StartLLVMCrucibleSetupParams n) =
  do pushTask (LLVMCrucibleSetup n [])
     ok

data StartLLVMCrucibleSetupParams =
  StartLLVMCrucibleSetupParams { llvmSetupName :: ServerName }

instance FromJSON StartLLVMCrucibleSetupParams where
  parseJSON =
    withObject "params for \"SAW/Crucible setup\"" $ \o ->
    StartLLVMCrucibleSetupParams <$> o .: "name"

data ServerSetupVal = Val (CMS.AllLLVM SetupValue)

interpretSetup :: BuiltinContext -> [SetupStep] -> LLVMCrucibleSetupM ()
interpretSetup bic ss = runStateT (traverse_ go (reverse ss)) mempty *> pure ()
  where
    go (SetupReturn v) = getSetupVal v >>= lift . crucible_return bic defaultOptions
    go (SetupFresh name debugName ty) =
      lift (crucible_fresh_var bic defaultOptions (T.unpack debugName) ty) >>=
      save name . Val . CMS.anySetupTerm
    go (SetupAlloc name ty) =
      lift (crucible_alloc bic defaultOptions ty) >>=
      save name . Val
    go (SetupPointsTo src tgt) =
      do ptr <- getSetupVal src
         tgt' <- getSetupVal tgt
         lift (crucible_points_to True bic defaultOptions ptr tgt')
    go (SetupExecuteFunction args) =
      traverse getSetupVal args >>= lift . crucible_execute_func bic defaultOptions

    save name val = modify' (Map.insert name val)

    getSetupVal NullPointer = return CMS.anySetupNull
    getSetupVal (ServerVal n) =
      resolve n >>=
      \case
        Val x -> return x -- TODO add cases for the server values that
                          -- are not coming from the setup monad
                          -- (e.g. surrounding context)
    getSetupVal (CryptolExpr expr) = return (CMS.anySetupTerm expr)

    resolve name =
      do env <- get
         case Map.lookup name env of
           Just v -> return v
           Nothing -> error "Server value not found - impossible!" -- rule out elsewhere

llvmCrucibleSetupDone :: NoParams -> Method SAWState OK
llvmCrucibleSetupDone NoParams =
  do tasks <- view sawTask <$> getState
     case tasks of
       ((LLVMCrucibleSetup n setup, _) : _) ->
         do dropTask
            bic <- view sawBIC <$> getState
            setServerVal n (interpretSetup bic setup)
            ok
       _ -> raise notSettingUpLLVMCrucible


data LLVMCrucibleReturnParams =
  LLVMCrucibleReturnParams { llvmReturnVal :: LLVMSetupVal Expression }

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
            val <- traverse getTypedTerm (llvmReturnVal v)
            modifyState $
              set sawTask ((LLVMCrucibleSetup n (SetupReturn val : setup), env) :
                           more)
            ok
       _ -> raise notSettingUpLLVMCrucible

data LLVMCrucibleFreshParams =
  LLVMCrucibleFreshParams { llvmFreshName :: ServerName, llvmFreshType :: Type }

instance FromJSON LLVMCrucibleFreshParams where
  parseJSON =
    withObject "params for \"SAW/LLVM/fresh\"" $ \o ->
    LLVMCrucibleFreshParams <$> o .: "name" <*> (llvmType <$> o .: "type")

llvmCrucibleFresh :: LLVMCrucibleFreshParams -> Method SAWState OK
llvmCrucibleFresh (LLVMCrucibleFreshParams serverName@(ServerName x) llvmType) =
  do tasks <- view sawTask <$> getState
     case tasks of
       ((LLVMCrucibleSetup n setup, env) : more) ->
         do bic <- view sawBIC <$> getState
            setServerVal serverName (crucible_fresh_var bic defaultOptions (T.unpack x) llvmType)
            ok
       _ -> raise notSettingUpLLVMCrucible

data LLVMCruciblePointsToParams =
  LLVMCruciblePointsToParams
    { llvmPointsToPointer :: LLVMSetupVal Expression
    , llvmPointsToTarget :: LLVMSetupVal Expression
    }

instance FromJSON LLVMCruciblePointsToParams where
  parseJSON =
    withObject "params for \"SAW/LLVM/points to\"" $ \o ->
    LLVMCruciblePointsToParams <$> o .: "pointer" <*> o .: "target"


llvmCruciblePointsTo :: LLVMCruciblePointsToParams -> Method SAWState OK
llvmCruciblePointsTo (LLVMCruciblePointsToParams ptr tgt) =
  do tasks <- view sawTask <$> getState
     case tasks of
       ((LLVMCrucibleSetup n setup, env) : more) ->
         do bic <- view sawBIC <$> getState
            ptr' <- traverse getTypedTerm ptr
            tgt' <- traverse getTypedTerm tgt
            modifyState $
              set sawTask ((LLVMCrucibleSetup n (SetupPointsTo ptr' tgt' : setup), env) :
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
         liftIO (LLVM.parseBitCodeFromFile fileName) >>=
         \case
           Left err -> raise (cantLoadLLVMModule (LLVM.formatError err))
           Right llvmMod ->
             do halloc <- getHandleAlloc
                Some mtrans <- liftIO $ stToIO $ Crucible.translateModule halloc llvmMod
                setServerVal serverName (Some (CMS.LLVMModule fileName llvmMod mtrans))
                ok
