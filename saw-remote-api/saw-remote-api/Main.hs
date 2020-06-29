{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as JSON
import Data.Text (Text)

import Argo
import Argo.CacheTree
import Argo.DefaultMain
import Argo.HistoryWrapper

import SAWServer
import SAWServer.CryptolSetup
import SAWServer.LLVMCrucibleSetup
import SAWServer.LLVMVerify
import SAWServer.ProofScript
import SAWServer.SaveTerm
import SAWServer.SetOption


main :: IO ()
main =
  do initSt <- initialState
     cache  <- newCache initSt
     theApp <- mkApp (HistoryWrapper cache) (historyWrapper validateSAWState sawMethods)
     defaultMain description theApp

description :: String
description =
  "An RPC server for SAW."

sawMethods :: [(Text, MethodType, JSON.Value -> Method SAWState JSON.Value)]
sawMethods =
  -- Cryptol
  [ ("SAW/Cryptol/load module",  Command, method cryptolLoadModule)
  , ("SAW/Cryptol/load file",    Command, method cryptolLoadFile)
  , ("SAW/Cryptol/save term",    Command, method saveTerm)
  -- LLVM
  , ("SAW/LLVM/load module",     Command, method llvmLoadModule)
  , ("SAW/LLVM/verify",          Command, method llvmVerify)
  , ("SAW/LLVM/verify assembly", Command, method llvmVerifyX86)
  , ("SAW/LLVM/assume",          Command, method llvmAssume)
  -- General
  , ("SAW/make simpset",         Command, method makeSimpset)
  , ("SAW/prove",                Command, method prove)
  , ("SAW/set option",           Command, method setOption)
  ]
