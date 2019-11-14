{-# LANGUAGE OverloadedStrings #-}

module SAWServer.LLVMVerify (llvmVerify) where

import Prelude hiding (mod)
import Control.Lens
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.=), (.:))
import Data.Parameterized.Pair
import Data.Parameterized.Some

import SAWScript.Builtins (satABC)
import SAWScript.Crucible.LLVM.Builtins (crucible_llvm_verify)
import SAWScript.Options (defaultOptions)
import SAWScript.Value (rwCryptol)

import Argo
import CryptolServer.Data.Expression
import SAWServer
import SAWServer.CryptolExpression (getTypedTerm)
import SAWServer.Exceptions
import SAWServer.LLVMCrucibleSetup
import SAWServer.OK
import SAWServer.TopLevel

data LLVMVerifyParams =
  LLVMVerifyParams
    { verifyModule       :: ServerName
    , verifyFunctionName :: String
    , verifyLemmas       :: [ServerName] --[SomeLLVM MS.CrucibleMethodSpecIR]
    , verifyCheckSat     :: Bool
    -- TODO: might want to be able to save contracts and refer to them by name?
    , verifyContract     :: Contract Expression -- ServerName
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
                     <*> o .: "contract"
                     <*> o .: "tactic"
                     <*> o .: "lemma name"

llvmVerify :: LLVMVerifyParams -> Method SAWState OK
llvmVerify (LLVMVerifyParams modName fun lemmaNames checkSat contract tactic lemmaName) =
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
            setup <- compileContract bic cenv <$> traverse getExpr contract
            -- TODO proof script - currently hard-coding abc
            res <- tl $ crucible_llvm_verify bic defaultOptions mod fun lemmas checkSat setup satABC
            dropTask
            setServerVal lemmaName res -- TODO: is this necessary?
            ok
