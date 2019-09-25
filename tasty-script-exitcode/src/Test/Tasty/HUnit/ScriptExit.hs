{-# LANGUAGE NamedFieldPuns #-}

module Test.Tasty.HUnit.ScriptExit where

import Test.Tasty
import Test.Tasty.HUnit
import System.Process
import System.Exit
import System.FilePath
import System.Directory
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
makeScriptTests scriptTestDir testLanguages = do
  scriptTestFiles <- map (scriptTestDir </>) <$> listDirectory scriptTestDir
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

-- | Given a list of @TestLang@s to use and a list of possible script
-- filespaths, generate a list of named tests corresponding to the exit-code
-- tests for each of the specified languages. This function has the same details
-- and caveats as 'makeScriptTests'.
perLanguageTests :: [TestLang] -> [FilePath] -> [TestTree]
perLanguageTests testLanguages =
  toTestTrees . foldr addScript Map.empty
  where
    languageSupport =
      Map.fromList $
        map (\TestLang{testLangName,
                       testLangExtension,
                       testLangExecutable,
                       testLangArgsFormat} ->
               (testLangExtension,
                (testLangName,
                 scriptTest testLangExecutable testLangArgsFormat)))
            testLanguages

    addScript :: FilePath -> Map String [TestTree] -> Map String [TestTree]
    addScript fileName =
      case Map.lookup (takeExtension fileName) languageSupport of
        Nothing -> id
        Just (language, makeTest) ->
          Map.insertWith (++) language [makeTest fileName]

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
  testCase (takeFileName scriptPath) $ do
    let args = makeArgs scriptPath
        process = proc execPath args
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode process ""
    case exitCode of
      ExitSuccess -> pure ()
      ExitFailure code ->
        assertFailure $
          "Exit code " <> show code <> ": "
          <> execPath <> " " <> concat (intersperse " " args)
          <> ":\nstdout: " <> stdout <> "\nstderr: " <> stderr
