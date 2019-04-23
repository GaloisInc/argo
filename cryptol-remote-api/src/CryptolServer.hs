{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CryptolServer where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Data.Foldable
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Cryptol.Eval.Monad (EvalOpts(..), PPOpts(..))
import Cryptol.ModuleSystem
  (ModuleCmd, ModuleEnv, ModuleError(..), ModuleWarning(..),
   checkExpr, evalExpr, loadModuleByPath, loadModuleByName)
import Cryptol.ModuleSystem.Env
  (getLoadedModules, lmCanonicalPath, lmFingerprint, meLoadedModules,
   initialModuleEnv, meSolverConfig)
import Cryptol.ModuleSystem.Fingerprint
import Cryptol.Parser.AST (ModName)
import Cryptol.Utils.Logger (quietLogger)
import Cryptol.Utils.PP (pretty, PP)

import Argo

runModuleCmd :: ModuleCmd a -> Method ServerState a
runModuleCmd cmd =
    do s   <- getState
       out <- liftIO $ cmd (theEvalOpts, view moduleEnv s)
       case out of
         (Left x, warns) ->
           raise (cryptolError x warns)
         (Right (x, newEnv), warns) ->
           do setState (set moduleEnv newEnv s)
              return x

data LoadedModule = LoadedModule
  { _loadedName :: Maybe ModName   -- ^ Working on this module.
  , _loadedPath :: FilePath        -- ^ Working on this file.
  }

loadedName :: Simple Lens LoadedModule (Maybe ModName)
loadedName = lens _loadedName (\v n -> v { _loadedName = n })

loadedPath :: Simple Lens LoadedModule FilePath
loadedPath = lens _loadedPath (\v n -> v { _loadedPath = n })


data ServerState =
  ServerState { _loadedModule :: Maybe LoadedModule
              , _moduleEnv :: ModuleEnv
              }

loadedModule :: Simple Lens ServerState (Maybe LoadedModule)
loadedModule = lens _loadedModule (\v n -> v { _loadedModule = n })

moduleEnv :: Simple Lens ServerState ModuleEnv
moduleEnv = lens _moduleEnv (\v n -> v { _moduleEnv = n })

initialState :: IO ServerState
initialState = ServerState Nothing <$> initialModuleEnv

theEvalOpts :: EvalOpts
theEvalOpts = EvalOpts quietLogger (PPOpts False 10 25)

-- | Check that all of the modules loaded in the Cryptol environment
-- currently have fingerprints that match those when they were loaded.
validateServerState :: ServerState -> IO Bool
validateServerState =
  foldr check (return True) . getLoadedModules . meLoadedModules . view moduleEnv
  where
    check lm continue =
      do fp <- fingerprintFile (lmCanonicalPath lm)
         if fp == Just (lmFingerprint lm)
           then continue
           else return False

--------------------------------------------------------------------------------

-- The schema for translating Cryptol errors/warnings into JSONRPCExceptions

-- Reserved range for Cryptol exceptions: 20,000 - 21,000
cryptolErrorBase :: Integer
cryptolErrorBase = 20000

cryptolError :: ModuleError -> [ModuleWarning] -> JSONRPCException
cryptolError err warns =
  makeJSONRPCException
    (cryptolErrorBase + errorNum)
    (Text.pack $ (pretty err) <> foldMap (\w -> "\n" <> pretty w) warns)
    (Just . JSON.object $ errorData ++ [("warnings", moduleWarnings warns)])
  where
    -- TODO: make sub-errors (parse, typecheck, etc.) into structured data so
    -- that another client can parse them and make use of them (possible
    -- locations noted below)

    (errorNum, errorData) = moduleError err

    moduleError err = case err of
      ModuleNotFound src path ->
        (0, [ ("source", jsonPretty src)
            , ("path", jsonList (map jsonString path))
            ])
      CantFindFile path ->
        (1, [ ("path", jsonString path)
            ])
      BadUtf8 path ue ->
        (2, [ ("path", jsonString path)
            , ("error", jsonShow ue)
            ])
      OtherIOError path exn ->
        (3, [ ("path", jsonString path)
            , ("error", jsonShow exn)
            ])
      ModuleParseError source err ->
        (4, [ ("source", jsonString source)
            , ("error", jsonShow err)
            ])
      RecursiveModules mods ->
        (5, [ ("modules", jsonList (reverse (map jsonPretty mods)))
            ])
      RenamerErrors src errs ->
        -- TODO: structured error here
        (6, [ ("source", jsonPretty src)
            , ("errors", jsonList (map jsonPretty errs))
            ])
      NoPatErrors src errs ->
        -- TODO: structured error here
        (7, [ ("source", jsonPretty src)
            , ("errors", jsonList (map jsonPretty errs))
            ])
      NoIncludeErrors src errs ->
        -- TODO: structured error here
        (8, [ ("source", jsonPretty src)
            , ("errors", jsonList (map jsonShow errs))
            ])
      TypeCheckingFailed src errs ->
        -- TODO: structured error here
        (9, [ ("source", jsonPretty src)
            , ("errors", jsonList (map jsonShow errs))
            ])
      ModuleNameMismatch expected found ->
        (10, [ ("expected", jsonPretty expected)
             , ("found", jsonPretty found)
             ])
      DuplicateModuleName name path1 path2 ->
        (11, [ ("name", jsonPretty name)
             , ("paths", jsonList [jsonString path1, jsonString path2])
             ])
      OtherFailure x ->
        (12, [ ("error", jsonString x)
             ])
      ImportedParamModule x ->
        (13, [ ("module", jsonPretty x)
             ])
      FailedToParameterizeModDefs x xs ->
        (14, [ ("module", jsonPretty x)
             , ("parameters", jsonList (map (jsonString . pretty) xs))
             ])
      NotAParameterizedModule x ->
        (15, [ ("module", jsonPretty x)
             ])
      ErrorInFile x y ->
        (n, ("path", jsonString x) : err)
        where (n, err) = moduleError y

    moduleWarnings :: [ModuleWarning] -> JSON.Value
    moduleWarnings =
      -- TODO: structured error here
      jsonList . concatMap
        (\w -> case w of
                TypeCheckWarnings tcwarns ->
                  map (jsonPretty . snd) tcwarns
                RenamerWarnings rnwarns ->
                  map jsonPretty rnwarns)

    -- Some little helpers for common ways of building JSON values in the above:

    jsonString :: String -> JSON.Value
    jsonString = JSON.String . Text.pack

    jsonPretty :: PP a => a -> JSON.Value
    jsonPretty = jsonString . pretty

    jsonShow :: Show a => a -> JSON.Value
    jsonShow = jsonString . show

    jsonList :: [JSON.Value] -> JSON.Value
    jsonList = JSON.Array . Vector.fromList
