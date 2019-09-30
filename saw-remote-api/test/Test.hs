{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.HUnit.ScriptExit
import Paths_saw_remote_api (getDataFileName)

main :: IO ()
main =
  do reqs <- getDataFileName "../python/requirements.txt"
     withPython3venv (Just reqs) $ \pip python ->
       do pySrc <- getDataFileName "../python"
          testScriptsDir <- getDataFileName "test-scripts/"
          pip ["install", pySrc]

          scriptTests <- makeScriptTests testScriptsDir [python]

          defaultMain $
            testGroup "Tests for saw-remote-api"
              [testGroup "Scripting API tests" scriptTests]
