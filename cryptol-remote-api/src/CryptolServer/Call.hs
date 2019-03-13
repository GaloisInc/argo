{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module CryptolServer.Call (Expression(..), Encoding(..), LetBinding(..), call) where

import Control.Applicative
import Control.Exception (throwIO)
import Control.Lens hiding ((.:), (.=))
import Control.Monad (guard, unless)
import Control.Monad.IO.Class
import Data.Aeson as JSON hiding (Encoding, Value, decode)
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Scientific as Sc
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import qualified Data.Vector as V
import Data.Text.Encoding (encodeUtf8)
import Numeric (showHex)

import Cryptol.Eval (evalSel)
import Cryptol.Eval.Monad
import Cryptol.Eval.Value
import Cryptol.IR.FreeVars (freeVars, FreeVars, tyDeps, valDeps)
import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, checkExpr, evalExpr, getPrimMap, loadModuleByPath, loadModuleByName, meLoadedModules)
import Cryptol.ModuleSystem.Env (initialModuleEnv, isLoadedParamMod, meSolverConfig)
import Cryptol.ModuleSystem.Name (NameInfo(Declared), nameInfo)
import Cryptol.Parser
import Cryptol.Parser.AST (Bind(..), BindDef(..), Decl(..), Expr(..), Type(..), PName(..), Ident(..), Literal(..), Named(..), NumInfo(..))
import Cryptol.Parser.Position (Located(..), emptyRange)
import Cryptol.Parser.Selector
import Cryptol.Prims.Syntax
import Cryptol.TypeCheck.AST (PrimMap, sType)
import Cryptol.TypeCheck.Solve (defaultReplExpr)
import Cryptol.TypeCheck.Subst (apSubst, listParamSubst)
import qualified Cryptol.TypeCheck.Type as TC
import Cryptol.Utils.Ident
import Cryptol.Utils.PP (pretty)
import qualified Cryptol.TypeCheck.Solver.SMT as SMT

import Argo.JSONRPC

import CryptolServer
import CryptolServer.Exceptions
import CryptolServer.Data.Expression
import CryptolServer.Data.Type


call :: CryptolServerQuery JSON.Value
call =
  cryptolCommandToQuery $
  do CallParams fun rawArgs <- params
     args <- traverse getExpr rawArgs
     let appExpr = mkEApp (EVar (UnQual (mkIdent fun))) args
     (expr, ty, schema) <- runModuleCmd (checkExpr appExpr)
     -- TODO: see Cryptol REPL for how to check whether we
     -- can actually evaluate things, which we can't do in
     -- a parameterized module
     evalAllowed ty
     evalAllowed schema
     me <- view moduleEnv <$> getState
     let cfg = meSolverConfig me
     perhapsDef <- liftIO $ SMT.withSolver cfg (\s -> defaultReplExpr s ty schema)
     case perhapsDef of
       Nothing -> error "TODO"
       Just (tys, checked) ->
         do noDefaults tys
            let su = listParamSubst tys
            let theType = apSubst su (sType schema)
            res <- runModuleCmd (evalExpr checked)
            prims <- runModuleCmd getPrimMap
            rid <- getRequestID
            val <- observe $ readBack rid prims theType res
            return (JSON.object [ "value" .= val
                                , "type string" .= pretty theType
                                , "type" .= JSONType mempty theType
                                ])

  where
    noDefaults [] = return ()
    noDefaults xs@(_:_) =
      do rid <- getRequestID
         raise (unwantedDefaults xs)


    evalAllowed x =
      do me <- view moduleEnv <$> getState
         let ds      = freeVars x
             badVals = foldr badName Set.empty (valDeps ds)
             bad     = foldr badName badVals (tyDeps ds)
             badName nm bs =
               case nameInfo nm of
                 Declared m _
                   | isLoadedParamMod m (meLoadedModules me) -> Set.insert nm bs
                 _ -> bs
         unless (Set.null bad) $
           raise (evalInParamMod (Set.toList bad))

readBack :: RequestID -> PrimMap -> TC.Type -> Value -> Eval Expression
readBack rid prims ty val =
  case TC.tNoUser ty of
    TC.TRec tfs ->
      Record . HM.fromList <$>
        sequence [ do fv <- evalSel val (RecordSel f Nothing)
                      fa <- readBack rid prims t fv
                      return (identText f, fa)
                 | (f, t) <- tfs
                 ]
    TC.TCon (TC (TCTuple _)) [] ->
      pure Unit
    TC.TCon (TC (TCTuple _)) ts ->
      Tuple <$> sequence [ do v <- evalSel val (TupleSel n Nothing)
                              a <- readBack rid prims t v
                              return a
                         | (n, t) <- zip [0..] ts
                         ]
    TC.TCon (TC TCBit) [] ->
      case val of
        VBit b -> pure (Bit b)
    TC.TCon (TC TCInteger) [] ->
      case val of
        VInteger i -> pure (Integer i)
    TC.TCon (TC TCSeq) [TC.tNoUser -> len, TC.tNoUser -> contents]
      | len == TC.tZero ->
        return Unit
      | contents == TC.TCon (TC TCBit) []
      , VWord _ wv <- val ->
        do BV w v <- wv >>= asWordVal
           return $ Num Hex (T.pack $ showHex v "") w
      | TC.TCon (TC (TCNum k)) [] <- len ->
        Sequence <$> sequence [ do v <- evalSel val (ListSel n Nothing)
                                   readBack rid prims contents v
                              | n <- [0 .. fromIntegral k]
                              ]
    other -> liftIO $ throwIO (invalidType other rid)


observe :: Eval a -> CryptolServerCommand a
observe (Ready x) = pure x
observe (Thunk f) = liftIO $ f theEvalOpts

mkEApp :: Expr PName -> [Expr PName] -> Expr PName
mkEApp f args = foldl EApp f args

data CallParams =
  CallParams
    { functionName :: Text
    , functionArgs :: [Expression]
    }

instance FromJSON CallParams where
  parseJSON =
    withObject "params for \"call\"" $
    \o -> CallParams <$> o .: "function" <*> o .: "arguments"

