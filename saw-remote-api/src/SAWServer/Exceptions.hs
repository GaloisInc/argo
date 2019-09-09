{-# LANGUAGE OverloadedStrings #-}
module SAWServer.Exceptions (
  -- * Environment errors
    serverValNotFound
  , notACryptolEnv
  , notAnLLVMModule
  , notAnLLVMSetup
  -- * Wrong monad errors
  , notSettingUpCryptol
  , notSettingUpLLVMCrucible
  , notAtTopLevel
  -- * LLVM errors
  , cantLoadLLVMModule
  -- * Verification
  , verificationException
  -- * To be eventually eliminated
  , genericError
  ) where

import Control.Exception
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Argo

serverValNotFound ::
  (ToJSON name, Show name) =>
  name {- ^ the name that was not found -}->
  JSONRPCException
serverValNotFound name =
  makeJSONRPCException 1001 ("No server value with name " <> T.pack (show name))
    (Just $ object ["name" .= name])

-- TODO: make error say what kind of thing it was
notACryptolEnv ::
  (ToJSON name, Show name) =>
  name {- ^ the name that should have been mapped to a Cryptol environment -}->
  JSONRPCException
notACryptolEnv name =
  makeJSONRPCException 1002
    ("The server value with name " <>
     T.pack (show name) <>
     " is not a Cryptol environment")
    (Just $ object ["name" .= name])

notAnLLVMModule ::
  (ToJSON name, Show name) =>
  name {- ^ the name that should have been mapped to an LLVM module -}->
  JSONRPCException
notAnLLVMModule name =
  makeJSONRPCException 1003
    ("The server value with name " <>
     T.pack (show name) <>
     " is not an LLVM module")
    (Just $ object ["name" .= name])

notAnLLVMSetup ::
  (ToJSON name, Show name) =>
  name {- ^ the name that should have been mapped to an LLVM setup script -}->
  JSONRPCException
notAnLLVMSetup name =
  makeJSONRPCException 1004
    ("The server value with name " <>
     T.pack (show name) <>
     " is not an LLVM setup script")
    (Just $ object ["name" .= name])



notSettingUpCryptol :: JSONRPCException
notSettingUpCryptol = makeJSONRPCException 1003 "Not currently setting up Cryptol" noData

notSettingUpLLVMCrucible :: JSONRPCException
notSettingUpLLVMCrucible = makeJSONRPCException 1004 "Not currently setting up Crucible/LLVM" noData

notAtTopLevel :: ToJSON a => a -> JSONRPCException
notAtTopLevel tasks = makeJSONRPCException 1005 "Not at top level" (Just tasks)

cantLoadLLVMModule :: String -> JSONRPCException
cantLoadLLVMModule err = makeJSONRPCException 5000 "Can't load LLVM module" (Just err)

verificationException :: Exception e => e -> JSONRPCException
verificationException e = makeJSONRPCException 6000 "Verification exception" (Just (displayException e))

noData :: Maybe ()
noData = Nothing


-- TODO: Get rid of these and make them specific
genericError :: Text -> JSONRPCException
genericError msg =
  makeJSONRPCException 100000 msg (Nothing :: Maybe ())
