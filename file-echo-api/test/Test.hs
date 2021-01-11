
module Main ( module Main ) where

import Test.Tasty
import Test.Tasty.HUnit.ScriptExit


import Argo.PythonBindings
import Paths_file_echo_api

import FileEchoServer()
-- ^ We import FileEchoServer to force rebuild when building
-- the tests in case changes have been made to server.

main :: IO ()
main =
  do reqs <- getArgoPythonFile "requirements.txt"
     withPython3venv (Just reqs) $ \pip python ->
       do pySrc <- getArgoPythonFile "."
          testScriptsDir <- getDataFileName "test-scripts/"
          pip ["install", pySrc]
          putStrLn "pipped"

          scriptTests <- makeScriptTests testScriptsDir [python]

          defaultMain $
            testGroup "Tests for file-echo-api"
              [ testGroup "Scripting API tests" scriptTests
              ]
