{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Tasty.HUnit.ScriptExit where

import Test.Tasty
import Test.Tasty.HUnit
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import System.Process
import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- | Given a directory path and a list of @TestLang@s to use, create a list of
-- named tests (one per @TestLang@), each of which when run will test all of the
-- script files for that particular language found in the given directory path.
-- A test is considered to have passed if it exits with code 0; otherwise, it
-- will fail and print the contents of its stdout and stderr as part of the test
-- failure message.
--
-- A note on duplicate @TestLang@ entries: if multiple @TestLang@s have the same
-- extension, only one of them will be used; if multiple @TestLang@s have the
-- same language name, they will be grouped together even if their extensions
-- differ (this may be desirable in the case that the scripting language can
-- interpret multiple different kinds of files but may need different
-- command-line setup to do so).
makeScriptTests :: FilePath -> [TestLang] -> IO [TestTree]
makeScriptTests scriptTestDir testLanguages =
  do scriptTestFiles <- map (scriptTestDir </>) <$> listDirectory scriptTestDir
     pure $ perLanguageTests testLanguages scriptTestFiles

-- | Defines the information necessary to run an exit-code test for another
-- language (usually a scripting language like Python).
data TestLang
  = TestLang
      { testLangName       :: String  -- ^ The language name (for test output)
      , testLangExtension  :: String  -- ^ The extension for its scripts
      , testLangExecutable :: String  -- ^ The executable to invoke
      , testLangArgsFormat :: FilePath -> [String]
        -- ^ Given a path to a script, how to list the arguments to the
        -- scripting language executable (usually this is just @\x -> [x]@ but
        -- not always)
      }

-- | The Python 3 scripting language: this definition assumes an executable
-- called @python3@ lives on your @$PATH@.
python3 :: TestLang
python3 =
  TestLang
    { testLangName       = "Python 3"
    , testLangExtension  = ".py"
    , testLangExecutable = "python3"
    , testLangArgsFormat = \file -> [file]
    }

mypy :: TestLang
mypy =
  TestLang
    { testLangName       = "MyPy (Python 3)"
    , testLangExtension  = ".py"
    , testLangExecutable = "mypy"
    , testLangArgsFormat = \file -> [file]
    }

-- | Python 3 in a virtual environment: this definition assumes that
-- an executable named @python3@ is present in @$PATH@. The first
-- argument is a description of the Python packages to make available
-- in the virtual environment, expressed in the form of a path to the
-- standard @requirements.txt@ file that lists the packages. The
-- second argument consists of the tests to be run in this
-- environment, which will be provided with a means of invoking @pip@
-- and a scripting language. The means of invoking @pip@ is a function
-- that, when provided with an argument list for @pip@, returns an
-- @IO@ action that provides the virtual environment's @pip@ with
-- those arguments. If @pip@ fails, the tests all fail.
withPython3venv ::
  Maybe FilePath {- ^ The path to requirements.txt, if desired -} ->
  (([String] -> IO ()) -> TestLang -> IO a) {- ^ The tests that run using the virtual environment, given pip and Python -}->
  IO a
withPython3venv requirements todo =
  withSystemTempDirectory "virtenv" $ \venvDir ->
  do mpy3 <- findExecutable "python3"
     mpy <- findExecutable "python"
     pyExe <- case mpy3 <|> mpy of
                Just exeName -> return exeName
                Nothing -> assertFailure "Python executable not found."
     let process = proc pyExe ["-m", "venv", venvDir]
         pipInstall = proc pyExe ["-m", "pip", "install", "--upgrade", "pip"]
     (exitCode, stdout, stderr) <- readCreateProcessWithExitCode process ""
     case exitCode of
       ExitFailure code ->
         assertFailure $
         "Failed to create virtualenv at \"" <> venvDir <> "\" "<>
         "with code " <> show code <> ": " <>
          ":\nstdout: " <> stdout <> "\nstderr: " <> stderr
       ExitSuccess ->
         let venvPython =
               TestLang
                 { testLangName       = "Python in virtualenv"
                 , testLangExtension  = ".py"
                 , testLangExecutable = venvDir </> "bin" </> "python"
                 , testLangArgsFormat = \file -> [file]
                 }
             pip args =
               let binDir = if os == "mingw32" then "Scripts" else "bin"
                   pipProc = proc (venvDir </> binDir </> "pip") args in
               readCreateProcessWithExitCode pipProc "" >>=
               \case
                 (ExitFailure code, pipStdout, pipStderr) ->
                   assertFailure $
                   "pip failed in environment \"" <> venvDir <> "\" "<>
                   "with code " <> show code <> ": " <>
                   ":\nstdout: " <> pipStdout <> "\nstderr: " <> pipStderr
                 (ExitSuccess, _, _) ->
                   pure ()
         in do (exitCode, stdout, stderr) <- readCreateProcessWithExitCode pipInstall ""
               case exitCode of
                 ExitFailure code ->
                   assertFailure $
                   "Failed to create install `pip` with code " <>
                   show code <> ": " <>
                   ":\nstdout: " <> stdout <> "\nstderr: " <> stderr
                 ExitSuccess -> do
                   traverse (\reqPath -> pip ["install", "-r", reqPath]) requirements
                   todo pip venvPython


-- | Given a list of @TestLang@s to use and a list of possible script
-- filespaths, generate a list of named tests corresponding to the exit-code
-- tests for each of the specified languages. This function has the same details
-- and caveats as 'makeScriptTests'.
perLanguageTests :: [TestLang] -> [FilePath] -> [TestTree]
perLanguageTests testLanguages =
  toTestTrees . foldr addScript Map.empty
  where
    languageSupport :: Map String [(String, FilePath -> TestTree)]
    languageSupport =
      Map.fromListWith (++) $
        map (\TestLang{testLangName,
                       testLangExtension,
                       testLangExecutable,
                       testLangArgsFormat} ->
               (testLangExtension,
                [(testLangName,
                  scriptTest testLangExecutable testLangArgsFormat)]))
            testLanguages

    addScript :: FilePath -> Map String [TestTree] -> Map String [TestTree]
    addScript fileName tests =
      case Map.lookup (takeExtension fileName) languageSupport of
        Nothing -> tests
        Just relevantLangs ->
          foldr
            (\(language, makeTest) ->
              Map.insertWith (++) language [makeTest fileName])
            tests
            relevantLangs

    toTestTrees :: Map String [TestTree] -> [TestTree]
    toTestTrees =
      map (uncurry testGroup) . Map.assocs

-- | Make an individual script test: given an executable name (either a full
-- path or some executable on the @$PATH@), a way to format an input script
-- filename as arguments, and an input script path, create a single unit test
-- which invokes the scripting language on the script file with an empty
-- @stdin@, and fails when it exits with some exit code other than 0. The result
-- of such test failures includes the exit code as well as the full contents of
-- the process's @stdout@ and @stderr@.
scriptTest :: FilePath -> (FilePath -> [String]) -> FilePath -> TestTree
scriptTest execPath makeArgs scriptPath =
  testCase (takeFileName scriptPath) $
    do let args = makeArgs scriptPath
           process = proc execPath args
       (exitCode, stdout, stderr) <- readCreateProcessWithExitCode process ""
       case exitCode of
         ExitSuccess -> pure ()
         ExitFailure code ->
           assertFailure $
             "Exit code " <> show code <> ": "
             <> execPath <> " " <> concat (intersperse " " args)
             <> ":\nstdout: " <> stdout <> "\nstderr: " <> stderr
