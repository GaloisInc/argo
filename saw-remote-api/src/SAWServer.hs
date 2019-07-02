{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module SAWServer where

import Control.Lens
import Control.Monad.ST
import Data.Aeson (FromJSON(..), ToJSON(..), fromJSON, withText)
import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Parameterized.Some
import Data.Text (Text)
import qualified Data.Text as T

import qualified Cryptol.TypeCheck.AST as Cryptol (Schema)
import qualified Lang.Crucible.FunctionHandle as Crucible (HandleAllocator, newHandleAllocator)
import qualified Verifier.Java.Codebase as JSS
import Verifier.SAW.CryptolEnv (CryptolEnv)
import Verifier.SAW.Module (emptyModule)
import Verifier.SAW.SharedTerm (Term, SharedContext, mkSharedContext, scLoadModule)
import Verifier.SAW.Term.Functor (mkModuleName)
import Verifier.SAW.TypedTerm (TypedTerm, CryptolModule)

import qualified SAWScript.Crucible.LLVM.MethodSpecIR as CMS (LLVMModule(..))
import SAWScript.Prover.Rewrite (basic_ss)
import SAWScript.Value (BuiltinContext(..), LLVMCrucibleSetupM)
import qualified Verifier.SAW.Cryptol.Prelude as CryptolSAW
import qualified Verifier.Java.SAWBackend as JavaSAW
import qualified Verifier.LLVM.Backend.SAW as LLVMSAW


import Argo

import SAWServer.Exceptions

type SAWCont = (SAWEnv, SAWTask)

data SAWTask
  = CryptolSetup ServerName CryptolEnv -- ^ The name to bind and the environment being built
  | ProofScript
  | LLVMCrucibleSetup ServerName (LLVMCrucibleSetupM ())

instance Show SAWTask where
  show (CryptolSetup n _) = "(CryptolSetup " ++ show n ++ " _)"
  show ProofScript = "ProofScript"
  show (LLVMCrucibleSetup n _) = "(LLVMCrucibleSetup" ++ show n ++ " _)"

data SAWState =
  SAWState
    { _sawEnv :: SAWEnv
    , _sawBIC :: BuiltinContext
    , _sawTask :: [(SAWTask, SAWEnv)]
    , _sawHandleAllocator :: Crucible.HandleAllocator RealWorld
    }

instance Show SAWState where
  show (SAWState e _ t _) = "(SAWState " ++ show e ++ " _sc_ " ++ show t ++ " _halloc_)"

sawEnv :: Simple Lens SAWState SAWEnv
sawEnv = lens _sawEnv (\v e -> v { _sawEnv = e })

sawBIC :: Simple Lens SAWState BuiltinContext
sawBIC = lens _sawBIC (\v bic -> v { _sawBIC = bic })

sawTask :: Simple Lens SAWState [(SAWTask, SAWEnv)]
sawTask = lens _sawTask (\v t -> v { _sawTask = t })

sawHandleAllocator :: Simple Lens SAWState (Crucible.HandleAllocator RealWorld)
sawHandleAllocator = lens _sawHandleAllocator (\v ha -> v { _sawHandleAllocator = ha })

pushTask :: SAWTask -> Method SAWState ()
pushTask t = modifyState mod
  where mod (SAWState env sc stack halloc) = SAWState env sc ((t, env) : stack) halloc

dropTask :: Method SAWState ()
dropTask = modifyState mod
  where mod (SAWState _ _ [] _) = error "Internal error - stack underflow"
        mod (SAWState _ sc ((t, env):stack) halloc) =
          SAWState env sc stack halloc

getHandleAlloc :: Method SAWState (Crucible.HandleAllocator RealWorld)
getHandleAlloc = view sawHandleAllocator <$> getState

initialState :: IO SAWState
initialState =
  do sc <- mkSharedContext
     CryptolSAW.scLoadPreludeModule sc
     JavaSAW.scLoadJavaModule sc
     LLVMSAW.scLoadLLVMModule sc
     CryptolSAW.scLoadCryptolModule sc
     let mn = mkModuleName ["SAWScript"]
     scLoadModule sc (emptyModule mn)
     ss <- basic_ss sc
     let jarFiles = []
         classPaths = []
     jcb <- JSS.loadCodebase jarFiles classPaths
     let bic = BuiltinContext { biSharedContext = sc
                              , biJavaCodebase = jcb
                              , biBasicSS = ss
                              }
     halloc <- stToIO $ Crucible.newHandleAllocator
     return (SAWState emptyEnv bic [] halloc)

validateSAWState :: SAWState -> IO Bool
validateSAWState _ = pure True


newtype SAWEnv =
  SAWEnv { sawEnvBindings :: Map ServerName ServerVal }
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
  | VLLVMCrucibleSetup (LLVMCrucibleSetupM ())
  | VLLVMModule (Some CMS.LLVMModule)

instance Show ServerVal where
  show (VTerm t) = "(VTerm " ++ show t ++ ")"
  show (VType t) = "(VType " ++ show t ++ ")"
  show (VCryptolModule _) = "VCryptolModule"
  show (VCryptolEnv _) = "VCryptolEnv"
  show (VLLVMCrucibleSetup _) = "VLLVMCrucibleSetup"
  show (VLLVMModule (Some _)) = "VLLVMModule"

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

instance IsServerVal (LLVMCrucibleSetupM ()) where
  toServerVal = VLLVMCrucibleSetup

instance IsServerVal (Some CMS.LLVMModule) where
  toServerVal = VLLVMModule

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
