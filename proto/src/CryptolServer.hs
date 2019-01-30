{-# LANGUAGE OverloadedStrings #-}
module CryptolServer where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON

import Cryptol.Eval.Monad (EvalOpts(..), PPOpts(..))
import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, checkExpr, evalExpr, loadModuleByPath, loadModuleByName)
import Cryptol.ModuleSystem.Env (initialModuleEnv, meSolverConfig)
import Cryptol.Parser.AST (ModName)
import Cryptol.Utils.Logger (quietLogger)
import Cryptol.Utils.PP (pretty)

import JSONRPC



badParams :: RequestID -> JSON.Value -> JSONRPCException
badParams rid params =
  JSONRPCException { errorCode = 2
                   , message = "Bad params"
                   , errorData = Just params
                   , errorID = Just rid
                   }

cantLoadMod :: RequestID -> JSON.Value -> JSONRPCException
cantLoadMod rid mod =
  JSONRPCException { errorCode = 3
                   , message = "Can't load module"
                   , errorData = Just mod
                   , errorID = Just rid
                   }

cryptolError :: RequestID -> JSON.Value -> JSONRPCException
cryptolError rid mod =
  JSONRPCException { errorCode = 4
                   , message = "Cryptol error"
                   , errorData = Just mod
                   , errorID = Just rid
                   }


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
            liftIO $ throwIO (badParams rid ps)
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
       s   <- getState
       out <- liftIO $ cmd (theEvalOpts, view moduleEnv s)
       case out of
         (Left x, warns) ->
           liftIO $ throwIO (cryptolError rid (JSON.toJSON (pretty x, map pretty warns)))
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
