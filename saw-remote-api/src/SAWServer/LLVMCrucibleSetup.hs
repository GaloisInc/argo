{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module SAWServer.LLVMCrucibleSetup
  ( startLLVMCrucibleSetup
  , llvmLoadModule
  , Contract(..)
  , ContractVar(..)
  , Allocated(..)
  , PointsTo(..)
  , compileContract
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import qualified Cryptol.Parser.AST as P
import Cryptol.Utils.Ident (mkIdent)
import qualified Data.LLVM.BitCode as LLVM
import SAWScript.Crucible.Common.MethodSpec as MS (SetupValue(..))
import SAWScript.Crucible.LLVM.Builtins
    ( crucible_alloc
    , crucible_execute_func
    , crucible_fresh_var
    , crucible_points_to
    , crucible_return
    , crucible_precond
    , crucible_postcond )
import qualified SAWScript.Crucible.LLVM.MethodSpecIR as CMS (AllLLVM, anySetupTerm, anySetupNull, loadLLVMModule)
import SAWScript.Options (defaultOptions)
import SAWScript.Value (BuiltinContext, LLVMCrucibleSetupM(..), biSharedContext)
import qualified Verifier.SAW.CryptolEnv as CEnv
import Verifier.SAW.CryptolEnv (CryptolEnv)
import Verifier.SAW.TypedTerm (TypedTerm)

import Argo
import SAWServer
import SAWServer.Data.LLVMType (JSONLLVMType, llvmType)
import SAWServer.CryptolExpression (getTypedTermOfCExp)
import SAWServer.Exceptions
import SAWServer.OK
import SAWServer.SetupValue ()

data Contract cryptolExpr =
  Contract
    { preVars       :: [ContractVar]
    , preConds      :: [cryptolExpr]
    , preAllocated  :: [Allocated]
    , prePointsTos  :: [PointsTo cryptolExpr]
    , argumentVals  :: [LLVMSetupVal cryptolExpr]
    , postVars      :: [ContractVar]
    , postConds     :: [cryptolExpr]
    , postAllocated :: [Allocated]
    , postPointsTos :: [PointsTo cryptolExpr]
    , returnVal     :: Maybe (LLVMSetupVal cryptolExpr)
    }
    deriving (Functor, Foldable, Traversable)

data ContractVar =
  ContractVar
    { contractVarServerName :: ServerName
    , contractVarName       :: Text
    , contractVarType       :: JSONLLVMType
    }

data Allocated =
  Allocated
    { allocatedServerName :: ServerName
    , allocatedType       :: JSONLLVMType
    }

data PointsTo cryptolExpr =
  PointsTo
    { pointer  :: LLVMSetupVal cryptolExpr
    , pointsTo :: LLVMSetupVal cryptolExpr
    } deriving (Functor, Foldable, Traversable)

instance FromJSON cryptolExpr => FromJSON (PointsTo cryptolExpr) where
  parseJSON =
    withObject "Points-to relationship" $ \o ->
      PointsTo <$> o .: "pointer"
               <*> o .: "points to"

instance FromJSON Allocated where
  parseJSON =
    withObject "LLVM allocated thing" $ \o ->
      Allocated <$> o .: "server name"
                <*> o .: "type"

instance FromJSON ContractVar where
  parseJSON =
    withObject "LLVM contract variable" $ \o ->
      ContractVar <$> o .: "server name"
                  <*> o .: "name"
                  <*> o .: "type"

instance FromJSON e => FromJSON (Contract e) where
  parseJSON =
    withObject "LLVM contract" $ \o ->
    Contract <$> o .: "pre vars"
             <*> o .: "pre conds"
             <*> o .: "pre allocated"
             <*> o .: "pre points tos"
             <*> o .: "argument vals"
             <*> o .: "post vars"
             <*> o .: "post conds"
             <*> o .: "post allocated"
             <*> o .: "post points tos"
             <*> o .: "return val"

startLLVMCrucibleSetup :: StartLLVMCrucibleSetupParams -> Method SAWState OK
startLLVMCrucibleSetup (StartLLVMCrucibleSetupParams n) =
  do pushTask (LLVMCrucibleSetup n [])
     ok

data StartLLVMCrucibleSetupParams
  = StartLLVMCrucibleSetupParams ServerName

instance FromJSON StartLLVMCrucibleSetupParams where
  parseJSON =
    withObject "params for \"SAW/Crucible setup\"" $ \o ->
    StartLLVMCrucibleSetupParams <$> o .: "name"

data ServerSetupVal = Val (CMS.AllLLVM SetupValue)

-- TODO: this is an extra layer of indirection that could be collapsed, but is easy to implement for now.
compileContract :: BuiltinContext -> CryptolEnv -> Contract (P.Expr P.PName) ->  LLVMCrucibleSetupM ()
compileContract bic cenv c = interpretSetup bic cenv (reverse steps)
  where
    setupFresh (ContractVar n dn ty) = SetupFresh n dn (llvmType ty)
    setupAlloc (Allocated   n    ty) = SetupAlloc n    (llvmType ty)
    steps =
      map setupFresh (preVars c) ++
      map SetupPrecond (preConds c) ++
      map setupAlloc (preAllocated c) ++
      map (\(PointsTo p v) -> SetupPointsTo p v) (prePointsTos c) ++
      [ SetupExecuteFunction (argumentVals c) ] ++
      map setupFresh (postVars c) ++
      map SetupPostcond (postConds c) ++
      map setupAlloc (postAllocated c) ++
      map (\(PointsTo p v) -> SetupPointsTo p v) (postPointsTos c) ++
      [ SetupReturn v | v <- maybeToList (returnVal c) ]

interpretSetup :: BuiltinContext -> CryptolEnv -> [SetupStep] -> LLVMCrucibleSetupM ()
interpretSetup bic cenv0 ss = runStateT (traverse_ go (reverse ss)) (mempty, cenv0) *> pure ()
  where
    go (SetupReturn v) = get >>= \env -> lift $ getSetupVal env v >>= crucible_return bic defaultOptions
    -- TODO: do we really want two names here?
    go (SetupFresh name@(ServerName n) debugName ty) =
      do t <- lift $ crucible_fresh_var bic defaultOptions (T.unpack debugName) ty
         (env, cenv) <- get
         put (env, CEnv.bindTypedTerm (mkIdent n, t) cenv)
         save name (Val (CMS.anySetupTerm t))
    go (SetupAlloc name ty) =
      lift (crucible_alloc bic defaultOptions ty) >>=
      save name . Val
    go (SetupPointsTo src tgt) = get >>= \env -> lift $
      do ptr <- getSetupVal env src
         tgt' <- getSetupVal env tgt
         crucible_points_to True bic defaultOptions ptr tgt'
    go (SetupExecuteFunction args) =
      get >>= \env ->
      lift $ traverse (getSetupVal env) args >>= crucible_execute_func bic defaultOptions
    go (SetupPrecond p) = get >>= \env -> lift $
      getTypedTerm env p >>= crucible_precond
    go (SetupPostcond p) = get >>= \env -> lift $
      getTypedTerm env p >>= crucible_postcond

    save name val = modify' (\(env, cenv) -> (Map.insert name val env, cenv))

    getSetupVal ::
      (Map ServerName ServerSetupVal, CryptolEnv) ->
      LLVMSetupVal (P.Expr P.PName) ->
      LLVMCrucibleSetupM (CMS.AllLLVM MS.SetupValue)
    getSetupVal _ NullPointer = LLVMCrucibleSetupM $ return CMS.anySetupNull
    getSetupVal (env, _) (ServerVal n) = LLVMCrucibleSetupM $
      resolve env n >>=
      \case
        Val x -> return x -- TODO add cases for the server values that
                          -- are not coming from the setup monad
                          -- (e.g. surrounding context)
    getSetupVal (_, cenv) (CryptolExpr expr) = LLVMCrucibleSetupM $
      do res <- liftIO $ getTypedTermOfCExp (biSharedContext bic) cenv expr
         -- TODO: add warnings (snd res)
         case fst res of
           Right (t, _) -> return (CMS.anySetupTerm t)
           Left err -> error $ "Cryptol error: " ++ show err -- TODO: report properly

    getTypedTerm ::
      (Map ServerName ServerSetupVal, CryptolEnv) ->
      P.Expr P.PName ->
      LLVMCrucibleSetupM TypedTerm
    getTypedTerm (_, cenv) expr = LLVMCrucibleSetupM $
      do res <- liftIO $ getTypedTermOfCExp (biSharedContext bic) cenv expr
         -- TODO: add warnings (snd res)
         case fst res of
           Right (t, _) -> return t
           Left err -> error $ "Cryptol error: " ++ show err -- TODO: report properly

    resolve env name =
       case Map.lookup name env of
         Just v -> return v
         Nothing -> error "Server value not found - impossible!" -- rule out elsewhere

data LLVMLoadModuleParams
  = LLVMLoadModuleParams ServerName FilePath

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
         do let ?laxArith = False -- TODO read from config
            halloc <- getHandleAlloc
            loaded <- liftIO (CMS.loadLLVMModule fileName halloc)
            case loaded of
              Left err -> raise (cantLoadLLVMModule (LLVM.formatError err))
              Right llvmMod ->
                do setServerVal serverName llvmMod
                   ok
