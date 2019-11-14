{-# LANGUAGE OverloadedStrings #-}
module SAWServer.Exceptions (
  -- * Environment errors
    serverValNotFound
  , notACryptolEnv
  , notAnLLVMModule
  , notAnLLVMSetup
  , notAnLLVMSetupVal
  , notAnLLVMMethodSpecIR
  -- * Wrong monad errors
  , notSettingUpCryptol
  , notSettingUpLLVMCrucible
  , notAtTopLevel
  -- * Cryptol errors
  , cryptolError
  -- * LLVM errors
  , cantLoadLLVMModule
  -- * Verification
  , verificationException
  ) where

import Control.Exception
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T

import Argo

serverValNotFound ::
  (ToJSON name, Show name) =>
  name {- ^ the name that was not found -}->
  JSONRPCException
serverValNotFound name =
  makeJSONRPCException 10000 ("No server value with name " <> T.pack (show name))
    (Just $ object ["name" .= name])

notACryptolEnv ::
  (ToJSON name, Show name) =>
  name {- ^ the name that should have been mapped to a Cryptol environment -}->
  JSONRPCException
notACryptolEnv name =
  makeJSONRPCException 10010
    ("The server value with name " <>
     T.pack (show name) <>
     " is not a Cryptol environment")
    (Just $ object ["name" .= name])

notAnLLVMModule ::
  (ToJSON name, Show name) =>
  name {- ^ the name that should have been mapped to an LLVM module -}->
  JSONRPCException
notAnLLVMModule name =
  makeJSONRPCException 10020
    ("The server value with name " <>
     T.pack (show name) <>
     " is not an LLVM module")
    (Just $ object ["name" .= name])

notAnLLVMSetup ::
  (ToJSON name, Show name) =>
  name {- ^ the name that should have been mapped to an LLVM setup script -}->
  JSONRPCException
notAnLLVMSetup name =
  makeJSONRPCException 10030
    ("The server value with name " <>
     T.pack (show name) <>
     " is not an LLVM setup script")
    (Just $ object ["name" .= name])

notAnLLVMSetupVal ::
  (ToJSON name, Show name) =>
  name {- ^ the name that should have been mapped to an LLVM setup value -}->
  JSONRPCException
notAnLLVMSetupVal name =
  makeJSONRPCException 10040
    ("The server value with name " <>
     T.pack (show name) <>
     " is not an LLVM setup value")
    (Just $ object ["name" .= name])

notAnLLVMMethodSpecIR ::
  (ToJSON name, Show name) =>
  name {- ^ the name that should have been mapped to a method specification IR -}->
  JSONRPCException
notAnLLVMMethodSpecIR name =
  makeJSONRPCException 10050
    ("The server value with name " <>
     T.pack (show name) <>
     " is not an LLVM method specification")
    (Just $ object ["name" .= name])

notSettingUpCryptol :: JSONRPCException
notSettingUpCryptol =
  makeJSONRPCException 10100 "Not currently setting up Cryptol" noData

notSettingUpLLVMCrucible :: JSONRPCException
notSettingUpLLVMCrucible =
  makeJSONRPCException
    10110 "Not currently setting up Crucible/LLVM" noData

notAtTopLevel :: ToJSON a => [a] -> JSONRPCException
notAtTopLevel tasks =
  makeJSONRPCException
    10120 "Not at top level"
    (Just (JSON.object ["tasks" .= tasks]))

cantLoadLLVMModule :: String -> JSONRPCException
cantLoadLLVMModule err =
  makeJSONRPCException
    10200 "Can't load LLVM module"
    (Just (JSON.object ["error" .= err]))

verificationException :: Exception e => e -> JSONRPCException
verificationException e =
  makeJSONRPCException
    10300 "Verification exception"
    (Just (JSON.object ["error" .= displayException e]))

noData :: Maybe ()
noData = Nothing
