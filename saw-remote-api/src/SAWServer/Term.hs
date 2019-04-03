{-# LANGUAGE OverloadedStrings #-}
module SAWServer.Term where

import Control.Applicative
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Verifier.SAW.Term.Functor

import SAWServer

newtype JSONModuleName = JSONModuleName ModuleName

instance FromJSON JSONModuleName where
  parseJSON v = literal v <|> structured v
    where
      literal =
        withText "module name as string" $
        pure . JSONModuleName . mkModuleName . map T.unpack . T.splitOn "."
      structured =
        withArray "module name as list of parts" $ \v ->
        do parts <- traverse parseJSON (V.toList v)
           pure $ JSONModuleName $ mkModuleName $ map T.unpack parts

instance ToJSON JSONModuleName where
  toJSON (JSONModuleName n) = toJSON (show n)
