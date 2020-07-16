{-# LANGUAGE OverloadedStrings #-}

module SAWServer.LLVMVerify
  ( llvmVerify
  , llvmVerifyX86
  , llvmAssume
  ) where

import Prelude hiding (mod)
import Control.Lens

import SAWScript.Crucible.LLVM.Builtins
import SAWScript.Crucible.LLVM.X86
import SAWScript.Options (defaultOptions)
import SAWScript.Value (rwCryptol)

import Argo
import CryptolServer.Data.Expression
import SAWServer
import SAWServer.Data.LLVMType
import SAWServer.Exceptions
import SAWServer.LLVMCrucibleSetup
import SAWServer.OK
import SAWServer.ProofScript
import SAWServer.TopLevel
import SAWServer.VerifyCommon

llvmVerify :: VerifyParams JSONLLVMType -> Method SAWState OK
llvmVerify (VerifyParams modName fun lemmaNames checkSat contract script lemmaName) =
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
            fileReader <- getFileReader
            setup <- compileLLVMContract fileReader bic cenv <$> traverse getExpr contract
            res <- tl $ crucible_llvm_verify bic defaultOptions mod fun lemmas checkSat setup proofScript
            dropTask
            setServerVal lemmaName res
            ok

llvmVerifyX86 :: X86VerifyParams JSONLLVMType -> Method SAWState OK
llvmVerifyX86 (X86VerifyParams modName objName fun globals _lemmaNames checkSat contract script lemmaName) =
  do tasks <- view sawTask <$> getState
     case tasks of
       (_:_) -> raise $ notAtTopLevel $ map fst tasks
       [] ->
         do pushTask (LLVMCrucibleSetup lemmaName [])
            state <- getState
            mod <- getLLVMModule modName
            let bic = view  sawBIC state
                cenv = rwCryptol (view sawTopLevelRW state)
                allocs = map (\(X86Alloc name size) -> (name, size)) globals
            proofScript <- interpretProofScript script
            fileReader <- getFileReader
            setup <- compileLLVMContract fileReader bic cenv <$> traverse getExpr contract
            res <- tl $ crucible_llvm_verify_x86 bic defaultOptions mod objName fun allocs checkSat setup proofScript
            dropTask
            setServerVal lemmaName res
            ok

llvmAssume :: AssumeParams JSONLLVMType -> Method SAWState OK
llvmAssume (AssumeParams modName fun contract lemmaName) =
  do tasks <- view sawTask <$> getState
     case tasks of
       (_:_) -> raise $ notAtTopLevel $ map fst tasks
       [] ->
         do pushTask (LLVMCrucibleSetup lemmaName [])
            state <- getState
            mod <- getLLVMModule modName
            let bic = view  sawBIC state
                cenv = rwCryptol (view sawTopLevelRW state)
            fileReader <- getFileReader
            setup <- compileLLVMContract fileReader bic cenv <$> traverse getExpr contract
            res <- tl $ crucible_llvm_unsafe_assume_spec bic defaultOptions mod fun setup
            dropTask
            setServerVal lemmaName res
            ok
