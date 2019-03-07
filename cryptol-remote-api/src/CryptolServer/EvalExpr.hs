{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.EvalExpr (evalExpression) where

import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory


import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, checkExpr, evalExpr, loadModuleByPath, loadModuleByName)
import Cryptol.ModuleSystem.Env (initialModuleEnv, meSolverConfig)
import Cryptol.Parser (parseExpr, parseModName)
import Cryptol.TypeCheck.AST (sType)
import Cryptol.TypeCheck.Solve (defaultReplExpr)
import Cryptol.TypeCheck.Subst (apSubst, listParamSubst)
import qualified Cryptol.TypeCheck.Solver.SMT as SMT

import CryptolServer
import Argo.JSONRPC

evalExpression :: Method ServerState
evalExpression =
  query . stateless $ \(EvalExprParams str) ->
    case parseExpr str of
      Left err -> raise (cryptolParseErr str err)
      Right e ->
        do (expr, ty, schema) <- runModuleCmd (checkExpr e)
          -- TODO: see Cryptol REPL for how to check whether we
          -- can actually evaluate things, which we can't do in
          -- a parameterized module
           me <- view moduleEnv <$> getState
           let cfg = meSolverConfig me
           perhapsDef <- liftIO $ SMT.withSolver cfg (\s -> defaultReplExpr s ty schema)
           case perhapsDef of
              Nothing -> error "TODO"
              Just (tys, checked) ->
                do -- TODO: warnDefaults here
                   let su = listParamSubst tys
                   let theType = apSubst su (sType schema)
                   res <- runModuleCmd (evalExpr checked)
                   return (JSON.toJSON (show res, show theType))

data EvalExprParams =
  EvalExprParams { evalExprExpression :: Text }

instance JSON.FromJSON EvalExprParams where
  parseJSON =
    JSON.withObject "params for \"evaluate expression\"" $
    \o -> EvalExprParams <$> o .: "expression"


cryptolParseErr ::
  (ToJSON expr, Show err) =>
  expr {- ^ the input that couldn't be parsed -} ->
  err {- ^ the parse error from Cryptol -} ->
  RequestID ->
  JSONRPCException
cryptolParseErr expr err rid =
  JSONRPCException { errorCode = 4
                   , message = "There was a Cryptol parse error."
                   , errorData = Just $ JSON.object ["input" .= expr, "error" .= show err]
                   , errorID = Just rid
                   }
