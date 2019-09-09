{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SAWServer.SetupValue where

import Control.Applicative
import Data.Aeson (FromJSON(..), withObject, withText, (.:))
import qualified Data.Text as T

import SAWScript.Crucible.Common.MethodSpec
import SAWScript.Crucible.LLVM.MethodSpecIR

import Argo
import CryptolServer.Data.Expression (Expression)
import SAWServer
import SAWServer.CryptolExpression


data LLVMSetupVal
  = NullPointer
  | CryptolExpr Expression

instance FromJSON LLVMSetupVal where
  parseJSON v =
    nullValue v <|> expression v

    where
      nullValue = withText "setup value text" $
        \case
          "null" -> pure NullPointer
          _ -> empty
      expression = fmap CryptolExpr . parseJSON


getSetupVal :: LLVMSetupVal -> Method SAWState (AllLLVM SetupValue)
getSetupVal NullPointer = return anySetupNull
getSetupVal (CryptolExpr e) = anySetupTerm <$> getTypedTerm e
