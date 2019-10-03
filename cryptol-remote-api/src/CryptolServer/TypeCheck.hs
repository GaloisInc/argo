{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.TypeCheck (checkType) where

import Control.Lens hiding ((.=))
import Data.Aeson as JSON


import Cryptol.ModuleSystem (checkExpr)
import Cryptol.ModuleSystem.Env (meSolverConfig)

import Argo

import CryptolServer
import CryptolServer.Exceptions
import CryptolServer.Data.Expression
import CryptolServer.Data.Type

checkType :: TypeCheckParams -> Method ServerState JSON.Value
checkType (TypeCheckParams e) =
  do e' <- getExpr e
     (_expr, _ty, schema) <- runModuleCmd (checkExpr e')
     _cfg <- meSolverConfig . view moduleEnv <$> getState
     return (JSON.object [ "type schema" .= JSONSchema schema ])
  where
    -- FIXME: Why is this check not being used?
    noDefaults [] = return ()
    noDefaults xs@(_:_) = raise (unwantedDefaults xs)

newtype TypeCheckParams =
  TypeCheckParams Expression

instance JSON.FromJSON TypeCheckParams where
  parseJSON =
    JSON.withObject "params for \"check type\"" $
    \o -> TypeCheckParams <$> o .: "expression"
