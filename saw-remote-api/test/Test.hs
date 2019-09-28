{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.FilePath ((</>))
import System.Process (system)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.HUnit.ScriptExit
import Paths_saw_remote_api (getDataFileName)

main :: IO ()
main = do
  venvDir <- getDataFileName "virtenv/"
  system $ "python3 -m venv \"" ++ venvDir ++ "\""

  reqs <- getDataFileName "../python/requirements.txt"
  pySrc <- getDataFileName "../python"


  testScriptsDir <- getDataFileName "test-scripts/"

  system $ (venvDir </> "bin" </> "pip") ++ " install -r \"" ++ reqs ++ "\""

  system $ (venvDir </> "bin" </> "pip") ++ " install  \"" ++ pySrc ++ "\""

  let venvPython =
        TestLang
          { testLangName       = "Python in virtualenv"
          , testLangExtension  = ".py"
          , testLangExecutable = venvDir </> "bin" </> "python"
          , testLangArgsFormat = \file -> [file]
          }


  scriptTests <- makeScriptTests testScriptsDir [venvPython]
  defaultMain $
    testGroup "Tests for saw-remote-api"
      [testGroup "Scripting API tests" scriptTests]
