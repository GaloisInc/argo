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
    nullValue v <|> fromObject v

    where
      nullValue = withText "setup value text" $
        \case
          "null" -> pure NullPointer
          _ -> empty
      fromObject = withObject "saved value or Cryptol expression" $ \o ->
        o .: "setup value" >>=
        \case
          SV -> ServerValue <$> o .: "name"
          C -> CryptolExpr <$> o .: "expression"
