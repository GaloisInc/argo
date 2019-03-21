{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.FocusedModule where

import Control.Lens hiding ((.:), (.=))
import Data.Aeson as JSON
import Control.Monad.IO.Class


import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, checkExpr, evalExpr, getPrimMap, loadModuleByPath, loadModuleByName, meFocusedModule, meLoadedModules)
import Cryptol.ModuleSystem.Env (initialModuleEnv, isLoadedParamMod, meSolverConfig)
import Cryptol.TypeCheck.AST (PrimMap, sType)
import Cryptol.TypeCheck.Solve (defaultReplExpr)
import Cryptol.TypeCheck.Subst (apSubst, listParamSubst)
import qualified Cryptol.TypeCheck.Solver.SMT as SMT
import Cryptol.Utils.PP

import Argo

import CryptolServer
import CryptolServer.Data.Expression
import CryptolServer.Data.Type

import Debug.Trace

focusedModule :: FocusedModParams -> Method ServerState JSON.Value
focusedModule _ =
  do me <- view moduleEnv <$> getState
     case meFocusedModule me of
       Nothing ->
         return $ JSON.object [ "module" .= JSON.Null ]
       Just name ->
         do let parameterized = isLoadedParamMod name (meLoadedModules me)
            return $ JSON.object [ "module" .= pretty name
                                 , "parameterized" .= parameterized
                                 ]


data FocusedModParams = FocusedModParams

instance JSON.FromJSON FocusedModParams where
  parseJSON _ = pure FocusedModParams
