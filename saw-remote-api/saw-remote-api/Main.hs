{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as JSON
import Data.Text (Text)

import Argo
import Argo.DefaultMain

import SAWServer
import SAWServer.CryptolSetup
import SAWServer.LLVMCrucibleSetup
import SAWServer.LLVMVerify
import SAWServer.ProofScript
import SAWServer.SaveTerm


main :: IO ()
main =
  do theApp <- mkApp initialState sawMethods
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
  , ("SAW/LLVM/assume",          Command, method llvmAssume)
  -- General
  , ("SAW/make simpset",         Command, method makeSimpset)
  , ("SAW/prove",                Command, method prove)
  ]
