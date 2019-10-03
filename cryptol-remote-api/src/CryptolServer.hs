{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CryptolServer where

import Control.Lens
import Control.Monad.IO.Class

import Cryptol.Eval.Monad (EvalOpts(..), PPOpts(..))
import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv)
import Cryptol.ModuleSystem.Env
  (getLoadedModules, lmCanonicalPath, lmFingerprint, meLoadedModules,
   initialModuleEnv, meSearchPath)
import Cryptol.ModuleSystem.Fingerprint
import Cryptol.Parser.AST (ModName)
import Cryptol.Utils.Logger (quietLogger)

import Argo
import CryptolServer.Exceptions

runModuleCmd :: ModuleCmd a -> Method ServerState a
runModuleCmd cmd =
    do s   <- getState
       out <- liftIO $ cmd (theEvalOpts, view moduleEnv s)
       case out of
         (Left x, warns) ->
           raise (cryptolError x warns)
         (Right (x, newEnv), warns) ->
           -- TODO: What to do about warnings when a command completes
           -- successfully?
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

setSearchPath :: [FilePath] -> ServerState -> ServerState
setSearchPath paths =
  over moduleEnv $ \me -> me { meSearchPath = paths ++ meSearchPath me }

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
