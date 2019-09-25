{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.HUnit.ScriptExit
import Paths_saw_remote_api (getDataFileName)

main :: IO ()
main = do
  testScriptsDir <- getDataFileName "test-scripts/"
  scriptTests <- makeScriptTests testScriptsDir [python3]
  defaultMain $
    testGroup "Tests for saw-remote-api"
      [testGroup "Scripting API tests" scriptTests]
