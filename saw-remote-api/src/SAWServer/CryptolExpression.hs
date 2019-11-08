{-# LANGUAGE PartialTypeSignatures #-}
module SAWServer.CryptolExpression where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Cryptol.Eval (EvalOpts(..), defaultPPOpts)
import Cryptol.ModuleSystem (ModuleRes)
import Cryptol.ModuleSystem.Base (genInferInput, getPrimMap, noPat, rename)
import Cryptol.ModuleSystem.Env (ModuleEnv)
import Cryptol.ModuleSystem.Interface (noIfaceParams)
import Cryptol.ModuleSystem.Monad (ModuleM, interactive, runModuleM, setNameSeeds, setSupply, typeCheckWarnings, typeCheckingFailed)
import qualified Cryptol.ModuleSystem.Renamer as MR
import Cryptol.Parser.Position (emptyRange, getLoc)
import Cryptol.TypeCheck (tcExpr)
import Cryptol.TypeCheck.Monad (InferOutput(..), inpVars, inpTSyns)
import Cryptol.Utils.Ident (interactiveName)
import Cryptol.Utils.Logger (quietLogger)
import Cryptol.Utils.PP
import SAWScript.Value (biSharedContext, TopLevelRW(..))
import Verifier.SAW.CryptolEnv
import Verifier.SAW.TypedTerm(TypedTerm(..))

import Argo
import CryptolServer.Data.Expression
import SAWServer
import SAWServer.Exceptions

getTypedTerm :: Expression -> Method SAWState TypedTerm
getTypedTerm inputExpr =
  do cenv <- rwCryptol . view sawTopLevelRW <$> getState
     expr <- getExpr inputExpr
     sc <- biSharedContext . view sawBIC <$> getState
     (t, _) <- moduleCmdResult =<< (liftIO $ getTypedTermOfCExp sc cenv expr)
     return t

getTypedTermOfCExp :: _ -> _ -> _ -> IO (ModuleRes TypedTerm)
getTypedTermOfCExp sc cenv expr =
  do let env = eModuleEnv cenv
     mres <- runModuleM (defaultEvalOpts, env) $
       do npe <- interactive (noPat expr) -- eliminate patterns

          -- resolve names
          let nameEnv = getNamingEnv cenv
          re <- interactive (rename interactiveName nameEnv (MR.rename npe))

          -- infer types
          let ifDecls = getAllIfaceDecls env
          let range = fromMaybe emptyRange (getLoc re)
          prims <- getPrimMap
          tcEnv <- genInferInput range prims noIfaceParams ifDecls
          let tcEnv' = tcEnv { inpVars = Map.union (eExtraTypes cenv) (inpVars tcEnv)
                             , inpTSyns = Map.union (eExtraTSyns cenv) (inpTSyns tcEnv)
                             }

          out <- liftIO (tcExpr re tcEnv')
          interactive (runInferOutput out)
     case mres of
       (Right ((checkedExpr, schema), modEnv'), ws) ->
         do let env' = cenv { eModuleEnv = modEnv' }
            trm <- liftIO $ translateExpr sc env' checkedExpr
            return (Right (TypedTerm schema trm, modEnv'), ws)
       (Left err, ws) -> return (Left err, ws)

liftModuleM :: ModuleEnv -> ModuleM a -> Method SAWState (a, ModuleEnv)
liftModuleM env m = liftIO (runModuleM (defaultEvalOpts, env) m) >>= moduleCmdResult

moduleCmdResult :: ModuleRes a -> Method SAWState (a, ModuleEnv)
moduleCmdResult (res, ws) =
  do mapM_ (liftIO . print . pp) ws
     case res of
       Right (a, me) -> return (a, me)
       Left err      -> raise $ genericError $ T.pack $ "Cryptol error:\n" ++ show (pp err) -- X.throwIO (ModuleSystemError err)

defaultEvalOpts :: EvalOpts
defaultEvalOpts = EvalOpts quietLogger defaultPPOpts

runInferOutput :: InferOutput a -> ModuleM a
runInferOutput out =
  case out of
    InferOK warns seeds supply o ->
      do setNameSeeds seeds
         setSupply supply
         typeCheckWarnings warns
         return o

    InferFailed warns errs ->
      do typeCheckWarnings warns
         typeCheckingFailed errs
