module Main where

import System.FilePath ((</>), takeBaseName)
import System.Directory
import Data.Traversable
import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.HUnit.ScriptExit

import Paths_argo_python
import Argo.PythonBindings

main :: IO ()
main =
  do reqs <- getArgoPythonFile "requirements.txt"
     withPython3venv (Just reqs) $ \pip python ->
       do pySrc <- getArgoPythonFile "."
          pip ["install", pySrc]

          thisDirectory <- getDataFileName "."

          subdirectories <-
            do paths <- listDirectory thisDirectory
               filterM (doesDirectoryExist . ("." </>)) paths

          allTests <- for subdirectories $ \dir ->
            do let name = takeBaseName dir
               tests <- makeScriptTests dir [mypy]
               pure (testGroup ("Typechecking: " <> name) tests)

          defaultMain $
            testGroup "Tests for Python components" allTests
