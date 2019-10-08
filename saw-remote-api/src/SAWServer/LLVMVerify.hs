{-# LANGUAGE OverloadedStrings #-}

module SAWServer.LLVMVerify (llvmVerify) where

import Prelude hiding (mod)
import Control.Lens
import Data.Aeson (FromJSON(..), withObject, (.:))

import SAWScript.Builtins (satABC)
import SAWScript.Crucible.LLVM.Builtins (crucible_llvm_verify)
import SAWScript.Options (defaultOptions)

import Argo
import SAWServer
import SAWServer.Exceptions
import SAWServer.OK
import SAWServer.TopLevel

data LLVMVerifyParams =
  LLVMVerifyParams
    { verifyModule       :: ServerName
    , verifyFunctionName :: String
    , verifyLemmas       :: [ServerName] --[SomeLLVM MS.CrucibleMethodSpecIR]
    , verifyCheckSat     :: Bool
    , verifySetup        :: ServerName
    , verifyTactic       :: ServerName
    , verifyLemmaName    :: ServerName
    }

instance FromJSON LLVMVerifyParams where
  parseJSON =
    withObject "SAW/LLVM/verify params" $ \o ->
    LLVMVerifyParams <$> o .: "module"
                     <*> o .: "function"
                     <*> o .: "lemmas"
                     <*> o .: "check sat"
                     <*> o .: "setup"
                     <*> o .: "tactic"
                     <*> o .: "lemma name"

llvmVerify :: LLVMVerifyParams -> Method SAWState OK
llvmVerify (LLVMVerifyParams{verifyModule = modName,
                             verifyFunctionName = fun,
                             verifyLemmas = _lemmas,
                             verifyCheckSat = checkSat,
                             verifySetup = setupName,
                             verifyTactic = _tactic,
                             verifyLemmaName = _lemmaName}) =
  do tasks <- view sawTask <$> getState
     case tasks of
       (_:_) -> raise $ notAtTopLevel $ map fst tasks
       [] ->
         do bic <- view sawBIC <$> getState
            mod <- getLLVMModule modName
            -- TODO lemmas
            -- TODO proof script - currently hard-coding abc
            setup <- getLLVMSetup setupName
            _res <- tl $ crucible_llvm_verify bic defaultOptions mod fun [] checkSat setup satABC
            -- TODO anything here?
            ok
