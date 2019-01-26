{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CryptolServer.Call (call) where

import Control.Applicative
import Control.Exception
import Control.Lens hiding ((.:))
import Control.Monad.IO.Class
import Data.Aeson as JSON hiding (Encoding)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import qualified Data.Vector as V
import Data.Text.Encoding (encodeUtf8)

import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, checkExpr, evalExpr, loadModuleByPath, loadModuleByName)
import Cryptol.ModuleSystem.Env (initialModuleEnv, meSolverConfig)
import Cryptol.Parser
import Cryptol.Parser.AST (Expr(..), Type(..), PName(..), Ident(..), Literal(..), Named(..), NumInfo(..))
import Cryptol.Parser.Position (Located(..), emptyRange)
import Cryptol.TypeCheck.AST (sType)
import Cryptol.TypeCheck.Solve (defaultReplExpr)
import Cryptol.TypeCheck.Subst (apSubst, listParamSubst)
import Cryptol.Utils.Ident
import qualified Cryptol.TypeCheck.Solver.SMT as SMT

import JSONRPC

import CryptolServer


-- Examples of data:
-- 0xff --> {"expression": "bits", "width": 8, "encoding": "hex", "value": "0xff"}
-- {x = 0xff, Y=zero} --> {"expression": "record", "fields": {"x": {expression: bits...}, "Y": ...}}

call :: CryptolServerCommand JSON.Value
call =
  do CallParams fun rawArgs <- params
     args <- traverse getExpr rawArgs
     let appExpr = mkEApp (EVar (UnQual (mkIdent fun))) args
     (expr, ty, schema) <- runModuleCmd (checkExpr appExpr)
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
      "hex" -> pure Hex
      "base64" -> pure Base64
      _ -> empty

data ArgSpec =
    Bit Bool
  | Num Encoding Text Integer -- ^ data and bitwidth
  | Record [(Text, ArgSpec)]
  | Sequence [ArgSpec]
  | Tuple [ArgSpec]

data ArgSpecTag = TagNum | TagRecord | TagSequence | TagTuple

instance JSON.FromJSON ArgSpecTag where
  parseJSON =
    withText "tag" $
    \case
      "bits"     -> pure TagNum
      "record"   -> pure TagRecord
      "sequence" -> pure TagSequence
      "tuple"    -> pure TagTuple
      _          -> empty

instance JSON.FromJSON ArgSpec where
  parseJSON v = bool v <|> obj v
    where
      bool =
        withBool "boolean" $ pure . Bit

      obj =
        withObject "argument" $
        \o -> o .: "expression" >>=
              \case
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
  where
    decode :: Encoding -> Text -> CryptolServerCommand Integer
    decode Base64 txt =
      let bytes = encodeUtf8 txt
      in
        case Base64.decode bytes of
          Left err -> do rid <- getRequestID
                         liftIO $ throwIO (invalidBase64 rid bytes err)
          Right decoded -> return $ bytesToInt decoded
    decode Hex txt =
      squish <$> traverse hexDigit (T.unpack txt)
      where squish = foldl (\acc i -> (acc * 256) + i) 0

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
    hexDigit c   = getRequestID >>= liftIO . throwIO . invalidHex c
getExpr (Record fields) =
  fmap ERecord $ for fields $
  \(name, spec) ->
    Named (Located emptyRange (mkIdent name)) <$> getExpr spec
getExpr (Sequence elts) =
  EList <$> traverse getExpr elts
getExpr (Tuple projs) =
  ETuple <$> traverse getExpr projs


invalidBase64 :: RequestID -> ByteString -> String -> JSONRPCException
invalidBase64 rid invalidData msg =
  JSONRPCException
    { errorCode = 32
    , message = T.pack msg
    , errorData = Just (JSON.toJSON (T.pack (show invalidData)))
    , errorID = Just rid
    }

invalidHex :: Char -> RequestID -> JSONRPCException
invalidHex invalidData rid =
  JSONRPCException
    { errorCode = 33
    , message = "Not a hex digit"
    , errorData = Just (JSON.toJSON (T.pack (show invalidData)))
    , errorID = Just rid
    }

-- TODO add tests that this is big-endian
-- | Interpret a ByteString as an integer
bytesToInt bs =
  BS.foldl' (\acc w -> (acc * 256) + toInteger w) 0 bs
