{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.TypeCheck where

import Control.Lens hiding ((.:), (.=))
import Data.Aeson as JSON
import Control.Monad.IO.Class


import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, checkExpr, evalExpr, getPrimMap, loadModuleByPath, loadModuleByName, meLoadedModules)
import Cryptol.ModuleSystem.Env (initialModuleEnv, isLoadedParamMod, meSolverConfig)
import Cryptol.TypeCheck.AST (PrimMap, sType)
import Cryptol.TypeCheck.Solve (defaultReplExpr)
import Cryptol.TypeCheck.Subst (apSubst, listParamSubst)
import qualified Cryptol.TypeCheck.Solver.SMT as SMT
import Cryptol.Utils.PP

import Argo.JSONRPC

import CryptolServer
import CryptolServer.Data.Expression
import CryptolServer.Data.Type

import Debug.Trace

checkType :: TypeCheckParams -> Method ServerState JSON.Value
checkType (TypeCheckParams e) =
  do e' <- getExpr e
     (expr, ty, schema) <- runModuleCmd (checkExpr e')
     cfg <- meSolverConfig . view moduleEnv <$> getState
     return (JSON.object [ "type schema" .= JSONSchema schema ])


  where
    noDefaults [] = return ()
    noDefaults xs@(_:_) = raise (unwantedDefaults xs)



data TypeCheckParams =
  TypeCheckParams
    { typeCheckExpr :: Expression }

instance JSON.FromJSON TypeCheckParams where
  parseJSON =
    JSON.withObject "params for \"check type\"" $
    \o -> TypeCheckParams <$> o .: "expression"
