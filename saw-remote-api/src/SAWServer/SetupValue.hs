{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SAWServer.SetupValue where

import Control.Applicative
import Data.Aeson (FromJSON(..), withObject, withText, (.:))
import qualified Data.Text as T


import SAWScript.Crucible.Common.MethodSpec
import SAWScript.Crucible.LLVM.MethodSpecIR
import Argo

data LLVMSetupVal = NullPointer

instance FromJSON LLVMSetupVal where
  parseJSON =
    nullValue

    where
      nullValue = withText "setup value text" $
        \case
          "null" -> pure NullPointer
          _ -> empty


getSetupVal :: LLVMSetupVal -> Method s (AllLLVM SetupValue)
getSetupVal NullPointer = return anySetupNull
