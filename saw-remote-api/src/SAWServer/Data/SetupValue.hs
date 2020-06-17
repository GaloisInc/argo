{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SAWServer.Data.SetupValue (CrucibleSetupVal) where

import Control.Applicative
import Data.Aeson (FromJSON(..), withObject, withText, (.:))

import SAWServer

data SetupValTag = SV | C

instance FromJSON SetupValTag where
  parseJSON =
    withText "tag for setup value"$
    \case
      "saved" -> pure SV
      "Cryptol" -> pure C
      _ -> empty

instance FromJSON cryptolExpr => FromJSON (CrucibleSetupVal cryptolExpr) where
  parseJSON v =
    nullValue v <|>
    (withObject "array value" $ \o -> ArrayValue <$> o .: "elements") v <|>
    -- (withObject "tuple value" $ \o -> TupleValue <$> o .: "elements") v <|>
    (withObject "field lvalue" $ \o -> FieldLValue <$> o .: "base" <*> o .: "field") v <|>
    (withObject "element lvalue" $ \o -> ElementLValue <$> o .: "base" <*> o .: "index") v <|>
    (withObject "global initializer" $ \o -> GlobalInitializer <$> o .: "name") v <|>
    (withObject "global lvalue" $ \o -> GlobalLValue <$> o .: "name") v <|>
    fromObject v

    where
      nullValue = withText "setup value text" $
        \case
          "null" -> pure NullValue
          _ -> empty
      fromObject = withObject "saved value or Cryptol expression" $ \o ->
        o .: "setup value" >>=
        \case
          SV -> ServerValue <$> o .: "name"
          C -> CryptolExpr <$> o .: "expression"
