{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SAWServer.SetupValue where

import Control.Applicative
import Data.Aeson (FromJSON(..), withObject, withText, (.:))
import Data.Parameterized.Pair
import qualified Data.Text as T

import SAWScript.Crucible.Common.MethodSpec
import SAWScript.Crucible.LLVM.MethodSpecIR
import SAWScript.Value (LLVMCrucibleSetupM)

import Argo
import CryptolServer.Data.Expression (Expression)
import SAWServer
import SAWServer.CryptolExpression
import SAWServer.Exceptions


data SetupValTag = SV | C

instance FromJSON SetupValTag where
  parseJSON =
    withText "tag for setup value"$
    \case
      "saved" -> pure SV
      "Cryptol" -> pure C
      _ -> empty

instance FromJSON cryptolExpr => FromJSON (LLVMSetupVal cryptolExpr) where
  parseJSON v =
    nullValue v <|> fromObject v

    where
      nullValue = withText "setup value text" $
        \case
          "null" -> pure NullPointer
          _ -> empty
      fromObject = withObject "saved value or Cryptol expression" $ \o ->
        o .: "setup value" >>=
        \case
          SV -> ServerVal <$> o .: "name"
          C -> CryptolExpr <$> o .: "expression"

getSetupVal ::
  LLVMSetupVal Expression ->
  Method SAWState (LLVMCrucibleSetupM (AllLLVM SetupValue))
getSetupVal NullPointer = return (return anySetupNull)
getSetupVal (ServerVal n) =
  getServerVal n >>=
  \case
    VLLVMCrucibleSetup (Pair TypedTermRepr val) ->
      return (anySetupTerm <$> val)
    _ -> raise (notAnLLVMSetupVal n)
getSetupVal (CryptolExpr e) = fmap return (anySetupTerm <$> getTypedTerm e)
