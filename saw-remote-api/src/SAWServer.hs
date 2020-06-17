{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module SAWServer
  ( module SAWServer
  ) where

import Prelude hiding (mod)
import Control.Lens
import Control.Monad.ST
import Data.Aeson (FromJSON(..), ToJSON(..), fromJSON, withText, (.:), withObject)
import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Parameterized.Pair
import Data.Parameterized.Some
import Data.Text (Text)
import qualified Data.Text as T
import qualified Crypto.Hash as Hash
import qualified Crypto.Hash.Conduit as Hash

import qualified Cryptol.Parser.AST as P
import qualified Cryptol.TypeCheck.AST as Cryptol (Schema)
import qualified Data.ABC.GIA as GIA
import qualified Lang.Crucible.FunctionHandle as Crucible (HandleAllocator, newHandleAllocator)
import qualified Lang.Crucible.JVM as CJ
import Text.LLVM.AST (Type)
import qualified Verifier.Java.Codebase as JSS
import Verifier.SAW.CryptolEnv (CryptolEnv)
import qualified Verifier.SAW.CryptolEnv as CryptolEnv
import Verifier.SAW.Module (emptyModule)
import Verifier.SAW.SharedTerm (mkSharedContext, scLoadModule, Term)
import Verifier.SAW.Term.Functor (mkModuleName)
import Verifier.SAW.TypedTerm (TypedTerm, CryptolModule)


import qualified SAWScript.Crucible.Common.MethodSpec as CMS (CrucibleMethodSpecIR)
import qualified SAWScript.Crucible.LLVM.MethodSpecIR as CMS (AllLLVM, SomeLLVM, LLVMModule(..))
import SAWScript.Options (defaultOptions)
import SAWScript.Position (Pos(..))
import SAWScript.Prover.Rewrite (basic_ss)
import SAWScript.Value (AIGProxy(..), BuiltinContext(..), LLVMCrucibleSetupM, TopLevelRO(..), TopLevelRW(..), defaultPPOpts)
import qualified Verifier.SAW.Cryptol.Prelude as CryptolSAW
import Verifier.SAW.CryptolEnv (initCryptolEnv, bindTypedTerm)
import Verifier.SAW.Rewriter (Simpset)
import qualified Verifier.Java.SAWBackend as JavaSAW
import qualified Verifier.LLVM.Backend.SAW as LLVMSAW
import qualified Cryptol.Utils.Ident as Cryptol

import Argo
import CryptolServer.Data.Expression
import qualified CryptolServer (validateServerState, ServerState(..))
import SAWServer.Exceptions

type SAWCont = (SAWEnv, SAWTask)

type CryptolAST = P.Expr P.PName

data SAWTask
  = ProofScriptTask
  | LLVMCrucibleSetup ServerName [SetupStep]

instance Show SAWTask where
  show ProofScriptTask = "ProofScript"
  show (LLVMCrucibleSetup n steps) = "(LLVMCrucibleSetup" ++ show n ++ " " ++ show steps ++ ")"


data CrucibleSetupVal e
  = NullValue
  | ArrayValue [CrucibleSetupVal e]
  -- | TupleValue [CrucibleSetupVal e]
  -- | RecordValue [(String, CrucibleSetupVal e)]
  | FieldLValue (CrucibleSetupVal e) String
  | ElementLValue (CrucibleSetupVal e) Int
  | GlobalInitializer String
  | GlobalLValue String
  | ServerValue ServerName
  | CryptolExpr e
  deriving (Foldable, Functor, Traversable)

data SetupStep
  = SetupReturn (CrucibleSetupVal CryptolAST) -- ^ The return value
  | SetupFresh ServerName Text Type -- ^ Server name to save in, debug name, fresh variable type
  | SetupAlloc ServerName Type Bool (Maybe Int) -- ^ Server name to save in, type of allocation, mutability, alignment
  | SetupPointsTo (CrucibleSetupVal CryptolAST) (CrucibleSetupVal CryptolAST) -- ^ Source, target
  | SetupExecuteFunction [CrucibleSetupVal CryptolAST] -- ^ Function's arguments
  | SetupPrecond CryptolAST -- ^ Function's precondition
  | SetupPostcond CryptolAST -- ^ Function's postcondition

instance Show SetupStep where
  show _ = "⟨SetupStep⟩" -- TODO


instance ToJSON SAWTask where
  toJSON = toJSON . show

data SAWState =
  SAWState
    { _sawEnv :: SAWEnv
    , _sawBIC :: BuiltinContext
    , _sawTask :: [(SAWTask, SAWEnv)]
    , _sawTopLevelRO :: TopLevelRO
    , _sawTopLevelRW :: TopLevelRW
    , _trackedFiles :: Map FilePath (Hash.Digest Hash.SHA256)
    }

instance Show SAWState where
  show (SAWState e _ t _ _ tf) = "(SAWState " ++ show e ++ " _sc_ " ++ show t ++ " _ro_ _rw" ++ show tf ++ ")"

sawEnv :: Lens' SAWState SAWEnv
sawEnv = lens _sawEnv (\v e -> v { _sawEnv = e })

sawBIC :: Lens' SAWState BuiltinContext
sawBIC = lens _sawBIC (\v bic -> v { _sawBIC = bic })

sawTask :: Lens' SAWState [(SAWTask, SAWEnv)]
sawTask = lens _sawTask (\v t -> v { _sawTask = t })

sawTopLevelRO :: Lens' SAWState TopLevelRO
sawTopLevelRO = lens _sawTopLevelRO (\v ro -> v { _sawTopLevelRO = ro })

sawTopLevelRW :: Lens' SAWState TopLevelRW
sawTopLevelRW = lens _sawTopLevelRW (\v rw -> v { _sawTopLevelRW = rw })

trackedFiles :: Lens' SAWState (Map FilePath (Hash.Digest Hash.SHA256))
trackedFiles = lens _trackedFiles (\v tf -> v { _trackedFiles = tf })


pushTask :: SAWTask -> Method SAWState ()
pushTask t = modifyState mod
  where mod (SAWState env bic stack ro rw tf) =
          SAWState env bic ((t, env) : stack) ro rw tf

dropTask :: Method SAWState ()
dropTask = modifyState mod
  where mod (SAWState _ _ [] _ _ _) = error "Internal error - stack underflow"
        mod (SAWState _ sc ((_t, env):stack) ro rw tf) =
          SAWState env sc stack ro rw tf

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
                , rwCrucibleAssertThenAssume = False
                , rwLaxArith = False
                , rwWhat4HashConsing = False
                }
     return (SAWState emptyEnv bic [] ro rw M.empty)

-- NOTE: KWF: 2020-04-22: This function could introduce a race condition: if a
-- file changes on disk after its hash is computed by validateSAWState, but
-- before the function returns and its result is used to inform whether to
-- recompute a cached result, the cached result may be used even if it is
-- associated with stale filesystem state. See the discussion of this issue at:
-- https://github.com/GaloisInc/argo/pull/70#discussion_r412462908
validateSAWState :: SAWState -> IO Bool
validateSAWState sawState =
  checkAll
    [ CryptolServer.validateServerState cryptolState
    , checkAll $ map (uncurry checkHash) (M.assocs (view trackedFiles sawState))
    ]
  where
    checkAll [] = pure True
    checkAll (c : cs) =
      do result <- c
         if result
           then checkAll cs
           else pure False

    checkHash path hash =
      do currentHash <- Hash.hashFile path
         pure (currentHash == hash)

    cryptolState =
      CryptolServer.ServerState Nothing
        (CryptolEnv.eModuleEnv . rwCryptol . view sawTopLevelRW $ sawState)


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

data CrucibleSetupTypeRepr :: * -> * where
  UnitRepr :: CrucibleSetupTypeRepr ()
  TypedTermRepr :: CrucibleSetupTypeRepr TypedTerm

data ServerVal
  = VTerm TypedTerm
  | VSimpset Simpset
  | VType Cryptol.Schema
  | VCryptolModule CryptolModule -- from SAW, includes Term mappings
  | VJVMClass JSS.Class
  -- | VJVMCrucibleSetup (Pair CrucibleSetupTypeRepr JVMSetupM)
  | VLLVMCrucibleSetup (Pair CrucibleSetupTypeRepr LLVMCrucibleSetupM)
  | VLLVMModule (Some CMS.LLVMModule)
  | VLLVMMethodSpecIR (CMS.SomeLLVM CMS.CrucibleMethodSpecIR)

instance Show ServerVal where
  show (VTerm t) = "(VTerm " ++ show t ++ ")"
  show (VSimpset ss) = "(VSimpset " ++ show ss ++ ")"
  show (VType t) = "(VType " ++ show t ++ ")"
  show (VCryptolModule _) = "VCryptolModule"
  show (VJVMClass _) = "VJVMClass"
  -- show (VJVMCrucibleSetup _) = "VJVMCrucibleSetup"
  show (VLLVMCrucibleSetup _) = "VLLVMCrucibleSetup"
  show (VLLVMModule (Some _)) = "VLLVMModule"
  show (VLLVMMethodSpecIR _) = "VLLVMMethodSpecIR"

class IsServerVal a where
  toServerVal :: a -> ServerVal

instance IsServerVal TypedTerm where
  toServerVal = VTerm

instance IsServerVal Simpset where
  toServerVal = VSimpset

instance IsServerVal Cryptol.Schema where
  toServerVal = VType

instance IsServerVal CryptolModule where
  toServerVal = VCryptolModule

instance IsServerVal (CMS.SomeLLVM CMS.CrucibleMethodSpecIR) where
  toServerVal = VLLVMMethodSpecIR

instance IsServerVal JSS.Class where
  toServerVal = VJVMClass

class KnownCrucibleSetupType a where
  knownCrucibleSetupRepr :: CrucibleSetupTypeRepr a

instance KnownCrucibleSetupType () where
  knownCrucibleSetupRepr = UnitRepr

instance KnownCrucibleSetupType TypedTerm where
  knownCrucibleSetupRepr = TypedTermRepr

instance KnownCrucibleSetupType a => IsServerVal (LLVMCrucibleSetupM a) where
  toServerVal x = VLLVMCrucibleSetup (Pair knownCrucibleSetupRepr x)

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

bindCryptolVar :: Text -> TypedTerm -> Method SAWState ()
bindCryptolVar x t =
  do modifyState $ over sawTopLevelRW $ \rw ->
       rw { rwCryptol = bindTypedTerm (Cryptol.mkIdent x, t) (rwCryptol rw) }

getJVMClass :: ServerName -> Method SAWState JSS.Class
getJVMClass n =
  do v <- getServerVal n
     case v of
       VJVMClass c -> return c
       _other -> raise (notAJVMClass n)

getLLVMModule :: ServerName -> Method SAWState (Some CMS.LLVMModule)
getLLVMModule n =
  do v <- getServerVal n
     case v of
       VLLVMModule m -> return m
       _other -> raise (notAnLLVMModule n)

getLLVMSetup :: ServerName -> Method SAWState (Pair CrucibleSetupTypeRepr LLVMCrucibleSetupM)
getLLVMSetup n =
  do v <- getServerVal n
     case v of
       VLLVMCrucibleSetup setup -> return setup
       _other -> raise (notAnLLVMSetup n)

getLLVMMethodSpecIR :: ServerName -> Method SAWState (CMS.SomeLLVM CMS.CrucibleMethodSpecIR)
getLLVMMethodSpecIR n =
  do v <- getServerVal n
     case v of
       VLLVMMethodSpecIR ir -> return ir
       _other -> raise (notAnLLVMMethodSpecIR n)

getSimpset :: ServerName -> Method SAWState Simpset
getSimpset n =
  do v <- getServerVal n
     case v of
       VSimpset ss -> return ss
       _other -> raise (notASimpset n)

getTerm :: ServerName -> Method SAWState TypedTerm
getTerm n =
  do v <- getServerVal n
     case v of
       VTerm t -> return t
       _other -> raise (notATerm n)
