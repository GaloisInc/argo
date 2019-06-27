{-# LANGUAGE OverloadedStrings #-}
module SAWServer where

import Control.Lens
import Data.Aeson (FromJSON(..), ToJSON(..), fromJSON, withText)
import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import qualified Cryptol.TypeCheck.AST as Cryptol (Schema)
import Verifier.SAW.CryptolEnv (CryptolEnv)
import Verifier.SAW.Module (emptyModule)
import Verifier.SAW.SharedTerm (Term, SharedContext, mkSharedContext, scLoadModule)
import Verifier.SAW.Term.Functor (mkModuleName)
import Verifier.SAW.TypedTerm (TypedTerm, CryptolModule)

import SAWScript.Value (CrucibleSetupM)
import qualified Verifier.SAW.Cryptol.Prelude as CryptolSAW
import qualified Verifier.Java.SAWBackend as JavaSAW
import qualified Verifier.LLVM.Backend.SAW as LLVMSAW


import Argo

import SAWServer.Exceptions

type SAWCont = (SAWEnv, SAWTask)

data SAWTask
  = CryptolSetup ServerName CryptolEnv -- ^ The name to bind and the environment being built
  | ProofScript
  | CrucibleSetup ServerName (CrucibleSetupM ())

instance Show SAWTask where
  show (CryptolSetup n _) = "(CryptolSetup " ++ show n ++ " _)"
  show ProofScript = "ProofScript"
  show (CrucibleSetup n _) = "(CrucibleSetup" ++ show n ++ " _)"

data SAWState =
  SAWState
    { _sawEnv :: SAWEnv
    , _sawSC :: SharedContext
    , _sawTask :: [(SAWTask, SAWEnv)]
    }

instance Show SAWState where
  show (SAWState e _ t) = "(SAWState " ++ show e ++ " _sc_ " ++ show t ++ ")"

sawEnv :: Simple Lens SAWState SAWEnv
sawEnv = lens _sawEnv (\v e -> v { _sawEnv = e })

sawSC :: Simple Lens SAWState SharedContext
sawSC = lens _sawSC (\v sc -> v { _sawSC = sc })

sawTask :: Simple Lens SAWState [(SAWTask, SAWEnv)]
sawTask = lens _sawTask (\v t -> v { _sawTask = t })

pushTask :: SAWTask -> Method SAWState ()
pushTask t = modifyState mod
  where mod (SAWState env sc stack) = SAWState env sc ((t, env) : stack)

dropTask :: Method SAWState ()
dropTask = modifyState mod
  where mod (SAWState _ _ []) = error "Internal error - stack underflow"
        mod (SAWState _ sc ((t, env):stack)) =
          SAWState env sc stack

initialState :: IO SAWState
initialState =
  do sc <- mkSharedContext
     CryptolSAW.scLoadPreludeModule sc
     JavaSAW.scLoadJavaModule sc
     LLVMSAW.scLoadLLVMModule sc
     CryptolSAW.scLoadCryptolModule sc
     let mn = mkModuleName ["SAWScript"]
     scLoadModule sc (emptyModule mn)

     return (SAWState emptyEnv sc [])

validateSAWState :: SAWState -> IO Bool
validateSAWState _ = pure True


newtype SAWEnv =
  SAWEnv { sawEnvBindings :: (Map ServerName ServerVal) }
  deriving Show

emptyEnv :: SAWEnv
emptyEnv = SAWEnv M.empty

newtype ServerName = ServerName Text
  deriving (Eq, Show, Ord)

instance ToJSON ServerName where
  toJSON (ServerName n) = toJSON n

instance FromJSON ServerName where
  parseJSON = withText "name" (pure . ServerName)


data ServerVal
  = VTerm TypedTerm
  | VType Cryptol.Schema
  | VCryptolModule CryptolModule -- from SAW, includes Term mappings
  | VCryptolEnv CryptolEnv  -- from SAW, includes Term mappings
  | VCrucibleSetup (CrucibleSetupM ())

instance Show ServerVal where
  show (VTerm t) = "(VTerm " ++ show t ++ ")"
  show (VType t) = "(VType " ++ show t ++ ")"
  show (VCryptolModule _) = "VCryptolModule"
  show (VCryptolEnv _) = "VCryptolEnv"

class IsServerVal a where
  toServerVal :: a -> ServerVal

instance IsServerVal TypedTerm where
  toServerVal = VTerm

instance IsServerVal Cryptol.Schema where
  toServerVal = VType

instance IsServerVal CryptolModule where
  toServerVal = VCryptolModule

instance IsServerVal CryptolEnv where
  toServerVal = VCryptolEnv

setServerVal :: IsServerVal val => ServerName -> val -> Method SAWState ()
setServerVal name val =
  do debugLog $ "Saving " <> (T.pack (show name))
     modifyState $
       over sawEnv $
       \(SAWEnv env) ->
         SAWEnv (M.insert name (toServerVal val) env)
     debugLog $ "Saved " <> (T.pack (show name))
     st <- getState
     debugLog $ "State is " <> T.pack (show st)


getServerVal :: ServerName -> Method SAWState ServerVal
getServerVal n =
  do SAWEnv serverEnv <- view sawEnv <$> getState
     st <- getState
     debugLog $ "Looking up " <> T.pack (show n) <> " in " <> T.pack (show st)
     case M.lookup n serverEnv of
       Nothing -> raise (serverValNotFound n)
       Just val -> return val

getCryptolEnv :: ServerName -> Method SAWState CryptolEnv
getCryptolEnv n =
  do v <- getServerVal n
     case v of
       VCryptolEnv env -> return env
       other -> raise (notACryptolEnv n)
