{-# LANGUAGE OverloadedStrings #-}

module SAWServer.VerifyCommon
  ( VerifyParams(..)
  , AssumeParams(..)
  ) where

import Prelude hiding (mod)
import Data.Aeson (FromJSON(..), withObject, (.:))

import CryptolServer.Data.Expression
import SAWServer
import SAWServer.Data.Contract
import SAWServer.ProofScript

data VerifyParams ty =
  VerifyParams
    { verifyModule       :: ServerName
    , verifyFunctionName :: String
    , verifyLemmas       :: [ServerName]
    , verifyCheckSat     :: Bool
    -- TODO: might want to be able to save contracts and refer to them by name?
    , verifyContract     :: Contract ty Expression -- ServerName
    -- TODO: might want to be able to save proof scripts and refer to them by name?
    , verifyScript       :: ProofScript
    , verifyLemmaName    :: ServerName
    }

instance (FromJSON ty) => FromJSON (VerifyParams ty) where
  parseJSON =
    withObject "SAW/verify params" $ \o ->
    VerifyParams <$> o .: "module"
                 <*> o .: "function"
                 <*> o .: "lemmas"
                 <*> o .: "check sat"
                 <*> o .: "contract"
                 <*> o .: "script"
                 <*> o .: "lemma name"

data AssumeParams ty =
  AssumeParams
    { assumeModule       :: ServerName
    , assumeFunctionName :: String
    -- TODO: might want to be able to save contracts and refer to them by name?
    , assumeContract     :: Contract ty Expression -- ServerName
    , assumeLemmaName    :: ServerName
    }

instance (FromJSON ty) => FromJSON (AssumeParams ty) where
  parseJSON =
    withObject "SAW/assume params" $ \o ->
    AssumeParams <$> o .: "module"
                 <*> o .: "function"
                 <*> o .: "contract"
                 <*> o .: "lemma name"
