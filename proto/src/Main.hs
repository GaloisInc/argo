{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as JSON
import Data.Text (Text)
import System.Directory (doesDirectoryExist, setCurrentDirectory)

import JSONRPC

import Cryptol.Eval () --(evalExpr)
import Cryptol.Eval.Monad (EvalOpts(..), PPOpts(..))
import Cryptol.Parser (parseExpr, parseModName)
import Cryptol.Parser.AST (ModName)
import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, checkExpr, evalExpr, loadModuleByPath, loadModuleByName)
import Cryptol.ModuleSystem.Env (initialModuleEnv, meSolverConfig)
import Cryptol.ModuleSystem.Monad (runModuleM)
import qualified Cryptol.TypeCheck.Solver.SMT as SMT
import Cryptol.TypeCheck.AST (sType)
import Cryptol.TypeCheck.Solve (defaultReplExpr)
import Cryptol.TypeCheck.Subst (apSubst, listParamSubst)
import Cryptol.Utils.Logger (quietLogger)

import Debug.Trace

main :: IO ()
main = realMain -- otherMain

realMain =
  do initSt <- initialState
     theApp <- mkApp initSt cryptolMethods
     serveStdIONS theApp
  where bogus = [("echo", Command (\r s p -> return (s, p)))]


otherMain = serveStdIONS =<< mkApp (0 :: Integer) [ ("get", Query $ \r s p -> return (JSON.toJSON s))
                                                  , ("increment", Command $ \r s p -> return (s + 1, JSON.toJSON s))
                                                  ]

cryptolMethods :: [(Text, Method ServerState)]
cryptolMethods =
  [ ("change directory", Command $ runCryptolServerCommand cd)
  , ("load module", Command $ runCryptolServerCommand loadModule)
  , ("evaluate expression", Command $ runCryptolServerCommand evalExpression)
  ]
  where

    cd =
      do (ChangeDirectoryParams newDir) <- params
         exists <- liftIO $ doesDirectoryExist newDir
         if exists
           then do liftIO $ setCurrentDirectory newDir
                   return (JSON.toJSON ())
           else do rid <- getRequestID
                   liftIO $ throwIO (dirNotFound rid newDir)

    loadModule =
      do (LoadModuleParams fn) <- params
         x <- runModuleCmd (loadModuleByPath fn)
         return (JSON.toJSON ())

    evalExpression =
      do (EvalExprParams str) <- params
         case parseExpr str of
            Left err -> do rid <- getRequestID; liftIO $ throwIO (cryptolParseErr rid str err)
            Right e ->
              do (expr, ty, schema) <- runModuleCmd (checkExpr e)
                 -- TODO: see Cryptol REPL for how to check whether we
                 -- can actually evaluate things, which we can't do in
                 -- a parameterized module
                 me <- view moduleEnv <$> getState
                 let cfg = meSolverConfig me
                 perhapsDef <- liftIO $ SMT.withSolver cfg (\s -> defaultReplExpr s ty schema)
                 case perhapsDef of
                   Nothing -> error "TODO"
                   Just (tys, def1) ->
                     do -- TODO: warnDefaults here
                        let su = listParamSubst tys
                        let theType = (apSubst su (sType schema))
                        res <- runModuleCmd (evalExpr def1)
                        return (JSON.toJSON (show res, show theType))


initialState :: IO ServerState
initialState = ServerState Nothing <$> initialModuleEnv


data ChangeDirectoryParams =
  ChangeDirectoryParams { newDirectory :: FilePath }

instance JSON.FromJSON ChangeDirectoryParams where
  parseJSON =
    JSON.withObject "params for \"change directory\"" $
    \o -> ChangeDirectoryParams <$> o .: "directory"


data LoadModuleParams =
  LoadModuleParams { loadModuleMod :: FilePath }

instance JSON.FromJSON LoadModuleParams where
  parseJSON =
    JSON.withObject "params for \"load module\"" $
    \o -> LoadModuleParams <$> o .: "file"


data EvalExprParams =
  EvalExprParams { evalExprExpression :: Text }

instance JSON.FromJSON EvalExprParams where
  parseJSON =
    JSON.withObject "params for \"evaluate expression\"" $
    \o -> EvalExprParams <$> o .: "expression"




newtype CryptolServerCommand a =
  CryptolServerCommand { runCryptolServerCommand ::  RequestID -> ServerState -> JSON.Value -> IO (ServerState, a) }

instance Functor CryptolServerCommand where
  fmap f (CryptolServerCommand g) =
    CryptolServerCommand $ \r s p -> do (s', x) <- g r s p; return (s', f x)

instance Applicative CryptolServerCommand where
  pure = return
  (<*>) = ap

instance Monad CryptolServerCommand where
  return x = CryptolServerCommand $ \r s p -> return (s, x)
  (CryptolServerCommand f) >>= g =
    CryptolServerCommand $ \r s p -> f r s p >>= \(s', x) -> runCryptolServerCommand (g x) r s' p

instance MonadIO CryptolServerCommand where
  liftIO m = CryptolServerCommand $ \r s p -> do x <- m ; return (s, x)

newtype CryptolServerQuery a =
  CryptolServerQuery { runCryptolServerQuery :: RequestID -> ServerState -> JSON.Value -> IO a }

instance Functor CryptolServerQuery where
  fmap f (CryptolServerQuery g) =
    CryptolServerQuery $ \r s p -> f <$> g r s p

instance Applicative CryptolServerQuery where
  pure = return
  (<*>) = ap

instance Monad CryptolServerQuery where
  return x = CryptolServerQuery $ \_ _ _ -> return x
  (CryptolServerQuery f) >>= g =
    CryptolServerQuery $ \r s p -> f r s p >>= \x -> runCryptolServerQuery (g x) r s p

instance MonadIO CryptolServerQuery where
  liftIO m = CryptolServerQuery $ \r s p -> m

newtype CryptolServerNotification a =
  CryptolServerNotification { runCryptolServerNotification :: JSON.Value -> ServerState -> IO (a, ServerState) }

instance Functor CryptolServerNotification where
  fmap f (CryptolServerNotification g) =
    CryptolServerNotification $ \p s -> do (x, s') <- g p s ; return (f x, s')

instance Applicative CryptolServerNotification where
  pure = return
  (<*>) = ap

instance Monad CryptolServerNotification where
  return x = CryptolServerNotification $ \p s -> return (x, s)
  (CryptolServerNotification f) >>= g =
    CryptolServerNotification $ \p s -> f p s >>= \(x, s') -> runCryptolServerNotification (g x) p s'


class HasServerState m where
  getState :: m ServerState

instance HasServerState CryptolServerCommand where
  getState = CryptolServerCommand $ \_ s _ -> return (s, s)

instance HasServerState CryptolServerQuery where
  getState = CryptolServerQuery $ \_ s _ -> return s

instance HasServerState CryptolServerNotification where
  getState = CryptolServerNotification $ \p s -> return (s, s)

class HasParams m where
  getParams :: m JSON.Value

instance HasParams CryptolServerCommand where
  getParams = CryptolServerCommand $ \r s p -> return (s, p)

instance HasParams CryptolServerQuery where
  getParams = CryptolServerQuery $ \r s p -> return p

instance HasParams CryptolServerNotification where
  getParams = CryptolServerNotification $ \p s -> return (p, s)

params :: (MonadIO m, HasRequestID m, HasParams m, JSON.FromJSON a) => m a
params =
  do ps <- getParams
     case JSON.fromJSON ps of
       JSON.Error msg ->
         do rid <- getRequestID
            throw (badParams rid ps)
       JSON.Success decoded -> return decoded

class HasRequestID m where
  getRequestID :: m RequestID

instance HasRequestID CryptolServerCommand where
  getRequestID = CryptolServerCommand $ \r s p -> return (s, r)

instance HasRequestID CryptolServerQuery where
  getRequestID = CryptolServerQuery $ \r s p -> return r

class SetsServerState m where
  modifyState :: (ServerState -> ServerState) -> m ()

instance SetsServerState CryptolServerCommand where
  modifyState f = CryptolServerCommand $ \r s p -> return (f s, ())

setState :: SetsServerState m => ServerState -> m ()
setState = modifyState . const

runModuleCmd :: (MonadIO m, HasRequestID m, HasServerState m, SetsServerState m) => ModuleCmd a -> m a
runModuleCmd cmd =
    do rid <- getRequestID
       s <- getState
       out <- liftIO $ cmd (theEvalOpts, view moduleEnv s)
       case out of
         (Left x, warns) ->
           liftIO $ throwIO (cantLoadMod rid (JSON.toJSON (show (x, warns))))
         (Right (x, newEnv), warns) ->
           do setState (set moduleEnv newEnv s)
              return x



theEvalOpts :: EvalOpts
theEvalOpts = EvalOpts quietLogger (PPOpts False 10 25)




parseParams v = (,) <$> JSON.parseJSON v <*> (JSON.withObject "state" (\o -> o .: "state") v)

data ServerHistory =
  HistLoadModule LoadModuleParams

instance JSON.FromJSON ServerHistory where
  parseJSON =
    JSON.withObject ("history entry") $
    \o -> do (cmd :: Text) <- o .: "command"
             case cmd of
               "load module" -> HistLoadModule <$> (o .: "params")
               _ -> empty

cantLoadMod :: RequestID -> JSON.Value -> JSONRPCException
cantLoadMod rid mod =
  JSONRPCException { errorCode = 3
                   , message = "Can't load module"
                   , errorData = Just mod
                   , errorID = Just rid
                   }

dirNotFound :: RequestID -> FilePath -> JSONRPCException
dirNotFound rid dir =
  JSONRPCException { errorCode = 3
                   , message = "Directory doesn't exist"
                   , errorData = Just (JSON.toJSON dir)
                   , errorID = Just rid
                   }


cryptolParseErr :: RequestID -> Text -> _ -> JSONRPCException
cryptolParseErr rid expr err =
  JSONRPCException { errorCode = 4
                   , message = "There was a Cryptol parse error."
                   , errorData = Just $ JSON.object ["input" .= expr, "error" .= show err]
                   , errorID = Just rid
                   }


badParams :: RequestID -> JSON.Value -> JSONRPCException
badParams rid params =
  JSONRPCException { errorCode = 2
                   , message = "Bad params"
                   , errorData = Just params
                   , errorID = Just rid
                   }

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
