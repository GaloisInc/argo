module SAWServer where

import Control.Lens
import Data.Aeson (FromJSON(..), ToJSON(..), fromJSON, withText)
import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)

import qualified Cryptol.TypeCheck.AST as Cryptol (Schema)
import Verifier.SAW.CryptolEnv (CryptolEnv)
import Verifier.SAW.SharedTerm (Term, SharedContext, mkSharedContext)
import Verifier.SAW.TypedTerm (TypedTerm, CryptolModule)

import Argo

import SAWServer.Exceptions

type SAWCont = (SAWEnv, SAWTask)

data SAWTask
  = CryptolSetup ServerName CryptolEnv -- ^ The name to bind and the environment being built
  | ProofScript
  | CrucibleSetup

data SAWState =
  SAWState
    { _sawEnv :: SAWEnv
    , _sawSC :: SharedContext
    , _sawTask :: [(SAWTask, SAWEnv)]
    }

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
     return (SAWState emptyEnv sc [])

validateSAWState :: SAWState -> IO Bool
validateSAWState _ = pure True


newtype SAWEnv =
  SAWEnv { sawEnvBindings :: (Map ServerName ServerVal) }

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
  | VCryptolEnv CryptolEnv -- from SAW, includes Term mappings
  | VCrucibleSetup

getServerVal :: ServerName -> Method SAWState ServerVal
getServerVal n =
  do SAWEnv serverEnv <- view sawEnv <$> getState
     case M.lookup n serverEnv of
       Nothing -> raise (serverValNotFound n)
       Just val -> return val

getCryptolEnv :: ServerName -> Method SAWState CryptolEnv
getCryptolEnv n =
  do v <- getServerVal n
     case v of
       VCryptolEnv env -> return env
       other -> raise (notACryptolEnv n)
