{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module CryptolServer.Call (call) where

import Control.Applicative
import Control.Exception (throwIO)
import Control.Lens hiding ((.:), (.=))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson as JSON hiding (Encoding, Value, decode)
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as HM
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
import Cryptol.Parser.AST (Expr(..), Type(..), PName(..), Ident(..), Literal(..), Named(..), NumInfo(..))
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

import JSONRPC

import CryptolServer



call :: CryptolServerCommand JSON.Value
call =
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
            return (JSON.object ["value" .= val, "type" .= pretty theType])

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

readBack :: RequestID -> PrimMap -> TC.Type -> Value -> Eval ArgSpec
readBack rid prims ty val =
  case TC.tNoUser ty of
    TC.TRec tfs ->
      Record <$> sequence [ do fv <- evalSel val (RecordSel f Nothing)
                               fa <- readBack rid prims t fv
                               return (identText f, fa)
                          | (f, t) <- tfs
                          ]
    TC.TCon (TC (TCTuple _)) ts ->
      Tuple <$> sequence [ do v <- evalSel val (TupleSel n Nothing)
                              a <- readBack rid prims t v
                              return a
                         | (n, t) <- zip [0..] ts
                         ]
    TC.TCon (TC TCBit) [] ->
      case val of
        VBit b -> pure (Bit b)
    TC.TCon (TC TCSeq) [len, contents]
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
    , functionArgs :: [ArgSpec]
    }

instance FromJSON CallParams where
  parseJSON =
    withObject "params for \"call\"" $
    \o -> CallParams <$> o .: "function" <*> o .: "arguments"

data Encoding = Base64 | Hex

instance JSON.FromJSON Encoding where
  parseJSON =
    withText "encoding" $
    \case
      "hex"    -> pure Hex
      "base64" -> pure Base64
      _        -> empty

data ArgSpec =
    Bit Bool
  | Unit
  | Num Encoding Text Integer -- ^ data and bitwidth
  | Record [(Text, ArgSpec)]
  | Sequence [ArgSpec]
  | Tuple [ArgSpec]

data ArgSpecTag = TagNum | TagRecord | TagSequence | TagTuple | TagUnit

instance JSON.FromJSON ArgSpecTag where
  parseJSON =
    withText "tag" $
    \case
      "bits"     -> pure TagNum
      "unit"     -> pure TagUnit
      "record"   -> pure TagRecord
      "sequence" -> pure TagSequence
      "tuple"    -> pure TagTuple
      _          -> empty

instance JSON.ToJSON ArgSpecTag where
  toJSON TagNum      = "bits"
  toJSON TagRecord   = "record"
  toJSON TagSequence = "sequence"
  toJSON TagTuple    = "tuple"

instance JSON.FromJSON ArgSpec where
  parseJSON v = bool v <|> obj v
    where
      bool =
        withBool "boolean" $ pure . Bit

      obj =
        withObject "argument" $
        \o -> o .: "expression" >>=
              \case
                TagUnit -> pure Unit
                TagNum ->
                  do enc <- o .: "encoding"
                     Num enc <$> o .: "data" <*> o .: "width"
                TagRecord ->
                  do fields <- o .: "data"
                     flip (withObject "record data") fields $
                       \fs -> Record . HM.toList <$> traverse parseJSON fs
                TagSequence ->
                  do contents <- o .: "data"
                     flip (withArray "sequence") contents $
                       \s -> Sequence . V.toList <$> traverse parseJSON s
                TagTuple ->
                  do contents <- o .: "data"
                     flip (withArray "tuple") contents $
                       \s -> Tuple . V.toList <$> traverse parseJSON s

instance ToJSON Encoding where
  toJSON Hex = String "hex"
  toJSON Base64 = String "base64"

instance JSON.ToJSON ArgSpec where
  toJSON Unit = object [ "expression" .= TagUnit ]
  toJSON (Bit b) = JSON.Bool b
  toJSON (Num enc dat w) =
    object [ "expression" .= TagNum
           , "data" .= String dat
           , "encoding" .= enc
           , "width" .= w
           ]
  toJSON (Record fields) =
    object [ "expression" .= TagRecord
           , "data" .= object [ name .= toJSON val
                              | (name, val) <- fields
                              ]
           ]
  toJSON (Sequence elts) =
    object [ "expression" .= TagSequence
           , "data" .= Array (V.fromList (map toJSON elts))
           ]
  toJSON (Tuple projs) =
    object [ "expression" .= TagTuple
           , "data" .= Array (V.fromList (map toJSON projs))
           ]


decode :: (HasRequestID m, MonadIO m) => Encoding -> Text -> m Integer
decode Base64 txt =
  let bytes = encodeUtf8 txt
  in
    case Base64.decode bytes of
      Left err ->
        do rid <- getRequestID
           raise (invalidBase64 bytes err)
      Right decoded -> return $ bytesToInt decoded
decode Hex txt =
  squish <$> traverse hexDigit (T.unpack txt)
  where
    squish = foldl (\acc i -> (acc * 16) + i) 0

hexDigit :: (Num a, HasRequestID m, MonadIO m) => Char -> m a
hexDigit '0' = pure 0
hexDigit '1' = pure 1
hexDigit '2' = pure 2
hexDigit '3' = pure 3
hexDigit '4' = pure 4
hexDigit '5' = pure 5
hexDigit '6' = pure 6
hexDigit '7' = pure 7
hexDigit '8' = pure 8
hexDigit '9' = pure 9
hexDigit 'a' = pure 10
hexDigit 'A' = pure 10
hexDigit 'b' = pure 11
hexDigit 'B' = pure 11
hexDigit 'c' = pure 12
hexDigit 'C' = pure 12
hexDigit 'd' = pure 13
hexDigit 'D' = pure 13
hexDigit 'e' = pure 14
hexDigit 'E' = pure 14
hexDigit 'f' = pure 15
hexDigit 'F' = pure 15
hexDigit c   = raise (invalidHex c)


getExpr :: ArgSpec -> CryptolServerCommand (Expr PName)
getExpr (Bit b) =
  return $
    ETyped
      (EVar (UnQual (mkIdent $ if b then "True" else "False")))
      TBit
getExpr (Num enc txt w) =
  do d <- decode enc txt
     return $ ETyped
       (ELit (ECNum d DecLit))
       (TSeq (TNum w) TBit)
getExpr (Record fields) =
  fmap ERecord $ for fields $
  \(name, spec) ->
    Named (Located emptyRange (mkIdent name)) <$> getExpr spec
getExpr (Sequence elts) =
  EList <$> traverse getExpr elts
getExpr (Tuple projs) =
  ETuple <$> traverse getExpr projs


invalidBase64 :: ByteString -> String -> CryptolServerException
invalidBase64 invalidData msg rid =
  JSONRPCException
    { errorCode = 32
    , message = T.pack msg
    , errorData = Just (JSON.toJSON (T.pack (show invalidData)))
    , errorID = Just rid
    }

invalidHex :: Char -> CryptolServerException
invalidHex invalidData rid =
  JSONRPCException
    { errorCode = 33
    , message = "Not a hex digit"
    , errorData = Just (JSON.toJSON (T.pack (show invalidData)))
    , errorID = Just rid
    }

invalidType :: TC.Type -> CryptolServerException
invalidType ty rid =
  JSONRPCException
    { errorCode = 34
    , message = "Can't convert Cryptol data from this type to JSON"
    , errorData = Just (JSON.toJSON (T.pack (show ty)))
    , errorID = Just rid
    }

unwantedDefaults defs rid =
  JSONRPCException
    { errorCode = 35
    , message = "Execution would have required these defaults"
    , errorData = Just (JSON.toJSON (T.pack (show defs)))
    , errorID = Just rid
    }

evalInParamMod mods rid =
  JSONRPCException
    { errorCode = 36
    , message = "Can't evaluate Cryptol in a parameterized module."
    , errorData = Just (toJSON (map pretty mods))
    , errorID = Just rid
    }

-- TODO add tests that this is big-endian
-- | Interpret a ByteString as an Integer
bytesToInt bs =
  BS.foldl' (\acc w -> (acc * 256) + toInteger w) 0 bs
