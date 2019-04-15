{-# LANGUAGE OverloadedStrings #-}
module SAWServer.SaveTerm where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.=), (.:))
import Data.Maybe
import qualified Data.Map as Map
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
import Verifier.SAW.CryptolEnv
import Verifier.SAW.TypedTerm(TypedTerm(..))

import Argo

import CryptolServer.Data.Expression
import SAWServer
import SAWServer.Exceptions
import SAWServer.OK

saveTerm :: SaveTermParams -> Method SAWState OK
saveTerm (SaveTermParams name envName e) =
  do expr <- getExpr e
     sc <- view sawSC <$> getState
     cenv <- getCryptolEnv envName
     let env = eModuleEnv cenv

     ((checkedExpr, schema), modEnv') <- liftModuleM env $
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

     let env' = cenv { eModuleEnv = modEnv' }
     trm <- liftIO $ translateExpr sc env' checkedExpr
     setServerVal name (TypedTerm schema trm)
     ok




data SaveTermParams =
  SaveTermParams
    { termName :: ServerName
    , termEnv :: ServerName
    , termExpr :: Expression
    }

instance FromJSON SaveTermParams where
  parseJSON =
    withObject "parameters for saving a term" $ \o ->
    SaveTermParams <$> o .: "name"
                   <*> o .: "cryptol setup"
                   <*> o .: "expression"


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
