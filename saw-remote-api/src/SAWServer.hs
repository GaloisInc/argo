{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module SAWServer
  ( module SAWServer
  ) where

import Prelude hiding (mod)
import Control.Lens
import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Parameterized.Some
import Data.Text (Text)
import qualified Data.Text as T

import qualified Cryptol.TypeCheck.AST as Cryptol (Schema)
import qualified Data.ABC.GIA as GIA
import qualified Lang.Crucible.FunctionHandle as Crucible (HandleAllocator, newHandleAllocator)
import qualified Lang.Crucible.JVM as CJ
import qualified Verifier.Java.Codebase as JSS
import Verifier.SAW.CryptolEnv (CryptolEnv)
import Verifier.SAW.Module (emptyModule)
import Verifier.SAW.SharedTerm (mkSharedContext, scLoadModule)
import Verifier.SAW.Term.Functor (mkModuleName)
import Verifier.SAW.TypedTerm (TypedTerm, CryptolModule)


import qualified SAWScript.Crucible.LLVM.MethodSpecIR as CMS (LLVMModule(..))
import SAWScript.Options (defaultOptions)
import SAWScript.Position (Pos(..))
import SAWScript.Prover.Rewrite (basic_ss)
import SAWScript.Value (AIGProxy(..), BuiltinContext(..), LLVMCrucibleSetupM, TopLevelRO(..), TopLevelRW(..), defaultPPOpts)
import qualified Verifier.SAW.Cryptol.Prelude as CryptolSAW
import Verifier.SAW.CryptolEnv (initCryptolEnv)
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

instance ToJSON SAWTask where
  toJSON = toJSON . show

data SAWState =
  SAWState
    { _sawEnv :: SAWEnv
    , _sawBIC :: BuiltinContext
    , _sawTask :: [(SAWTask, SAWEnv)]
    , _sawTopLevelRO :: TopLevelRO
    , _sawTopLevelRW :: TopLevelRW
    }

instance Show SAWState where
  show (SAWState e _ t _ _) = "(SAWState " ++ show e ++ " _sc_ " ++ show t ++ " _ro_ _rw)"

sawEnv :: Simple Lens SAWState SAWEnv
sawEnv = lens _sawEnv (\v e -> v { _sawEnv = e })

sawBIC :: Simple Lens SAWState BuiltinContext
sawBIC = lens _sawBIC (\v bic -> v { _sawBIC = bic })

sawTask :: Simple Lens SAWState [(SAWTask, SAWEnv)]
sawTask = lens _sawTask (\v t -> v { _sawTask = t })

sawTopLevelRO :: Simple Lens SAWState TopLevelRO
sawTopLevelRO = lens _sawTopLevelRO (\v ro -> v { _sawTopLevelRO = ro })

sawTopLevelRW :: Simple Lens SAWState TopLevelRW
sawTopLevelRW = lens _sawTopLevelRW (\v rw -> v { _sawTopLevelRW = rw })


pushTask :: SAWTask -> Method SAWState ()
pushTask t = modifyState mod
  where mod (SAWState env bic stack ro rw) = SAWState env bic ((t, env) : stack) ro rw

dropTask :: Method SAWState ()
dropTask = modifyState mod
  where mod (SAWState _ _ [] _ _) = error "Internal error - stack underflow"
        mod (SAWState _ sc ((_t, env):stack) ro rw) =
          SAWState env sc stack ro rw

getHandleAlloc :: Method SAWState Crucible.HandleAllocator
getHandleAlloc = roHandleAlloc . view sawTopLevelRO <$> getState

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
     cenv <- initCryptolEnv sc
     halloc <- Crucible.newHandleAllocator
     jvmTrans <- CJ.mkInitialJVMContext halloc
     let ro = TopLevelRO
                { roSharedContext = sc
                , roJavaCodebase = jcb
                , roOptions = defaultOptions
                , roHandleAlloc = halloc
                , roPosition = PosInternal "SAWServer"
                , roProxy = AIGProxy GIA.proxy
                }
         rw = TopLevelRW
                { rwValues = mempty
                , rwTypes = mempty
                , rwTypedef = mempty
                , rwDocs = mempty
                , rwCryptol = cenv
                , rwPPOpts = defaultPPOpts
                , rwJVMTrans = jvmTrans
                , rwPrimsAvail = mempty
                , rwSMTArrayMemoryModel = False
                , rwProfilingFile = Nothing
                }
     return (SAWState emptyEnv bic [] ro rw)

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
       _other -> raise (notACryptolEnv n)

getLLVMModule :: ServerName -> Method SAWState (Some CMS.LLVMModule)
getLLVMModule n =
  do v <- getServerVal n
     case v of
       VLLVMModule m -> return m
       _other -> raise (notAnLLVMModule n)

getLLVMSetup :: ServerName -> Method SAWState (LLVMCrucibleSetupM ())
getLLVMSetup n =
  do v <- getServerVal n
     case v of
       VLLVMCrucibleSetup setup -> return setup
       _other -> raise (notAnLLVMSetup n)
