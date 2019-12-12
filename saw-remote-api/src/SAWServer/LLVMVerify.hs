{-# LANGUAGE OverloadedStrings #-}

module SAWServer.LLVMVerify
  ( llvmVerify
  , llvmAssume
  , LLVMVerifyParams(..)
  , LLVMAssumeParams(..)
  ) where

import Prelude hiding (mod)
import Control.Lens
import Data.Aeson (FromJSON(..), withObject, (.:))

import SAWScript.Crucible.LLVM.Builtins
import SAWScript.Options (defaultOptions)
import SAWScript.Value (rwCryptol)

import Argo
import CryptolServer.Data.Expression
import SAWServer
import SAWServer.Exceptions
import SAWServer.LLVMCrucibleSetup
import SAWServer.OK
import SAWServer.ProofScript
import SAWServer.TopLevel

data LLVMVerifyParams =
  LLVMVerifyParams
    { verifyModule       :: ServerName
    , verifyFunctionName :: String
    , verifyLemmas       :: [ServerName] --[SomeLLVM MS.CrucibleMethodSpecIR]
    , verifyCheckSat     :: Bool
    -- TODO: might want to be able to save contracts and refer to them by name?
    , verifyContract     :: Contract Expression -- ServerName
    -- TODO: might want to be able to save proof scripts and refer to them by name?
    , verifyScript       :: ProofScript
    , verifyLemmaName    :: ServerName
    }

instance FromJSON LLVMVerifyParams where
  parseJSON =
    withObject "SAW/LLVM/verify params" $ \o ->
    LLVMVerifyParams <$> o .: "module"
                     <*> o .: "function"
                     <*> o .: "lemmas"
                     <*> o .: "check sat"
                     <*> o .: "contract"
                     <*> o .: "script"
                     <*> o .: "lemma name"

data LLVMAssumeParams =
  LLVMAssumeParams
    { assumeModule       :: ServerName
    , assumeFunctionName :: String
    -- TODO: might want to be able to save contracts and refer to them by name?
    , assumeContract     :: Contract Expression -- ServerName
    , assumeLemmaName    :: ServerName
    }

instance FromJSON LLVMAssumeParams where
  parseJSON =
    withObject "SAW/LLVM/assume params" $ \o ->
    LLVMAssumeParams <$> o .: "module"
                     <*> o .: "function"
                     <*> o .: "contract"
                     <*> o .: "lemma name"

llvmVerify :: LLVMVerifyParams -> Method SAWState OK
llvmVerify (LLVMVerifyParams modName fun lemmaNames checkSat contract script lemmaName) =
  do tasks <- view sawTask <$> getState
     case tasks of
       (_:_) -> raise $ notAtTopLevel $ map fst tasks
       [] ->
         do pushTask (LLVMCrucibleSetup lemmaName [])
            state <- getState
            mod <- getLLVMModule modName
            lemmas <- mapM getLLVMMethodSpecIR lemmaNames
            let bic = view  sawBIC state
                cenv = rwCryptol (view sawTopLevelRW state)
            proofScript <- interpretProofScript script
            setup <- compileContract bic cenv <$> traverse getExpr contract
            res <- tl $ crucible_llvm_verify bic defaultOptions mod fun lemmas checkSat setup proofScript
            dropTask
            setServerVal lemmaName res
            ok

llvmAssume :: LLVMAssumeParams -> Method SAWState OK
llvmAssume (LLVMAssumeParams modName fun contract lemmaName) =
  do tasks <- view sawTask <$> getState
     case tasks of
       (_:_) -> raise $ notAtTopLevel $ map fst tasks
       [] ->
         do pushTask (LLVMCrucibleSetup lemmaName [])
            state <- getState
            mod <- getLLVMModule modName
            let bic = view  sawBIC state
                cenv = rwCryptol (view sawTopLevelRW state)
            setup <- compileContract bic cenv <$> traverse getExpr contract
            res <- tl $ crucible_llvm_unsafe_assume_spec bic defaultOptions mod fun setup
            dropTask
            setServerVal lemmaName res
            ok
