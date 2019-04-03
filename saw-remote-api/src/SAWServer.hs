module SAWServer where

import Control.Lens
import Data.Aeson (FromJSON(..), ToJSON(..), fromJSON, withText)
import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Verifier.SAW.SharedTerm (Term)

import Argo


data SAWState =
  SAWState
    { _sawEnv :: SAWEnv
    }

sawEnv :: Simple Lens SAWState SAWEnv
sawEnv = lens _sawEnv (\v e -> v { _sawEnv = e })

initialState :: IO SAWState
initialState = pure (SAWState emptyEnv)

validateServerState :: SAWState -> IO Bool
validateServerState _ = pure True


newtype SAWEnv = SAWEnv (Map ServerName ServerVal)

emptyEnv :: SAWEnv
emptyEnv = SAWEnv M.empty

newtype ServerName = ServerName Text
  deriving (Eq, Show, Ord)

instance ToJSON ServerName where
  toJSON (ServerName n) = toJSON n

instance FromJSON ServerName where
  parseJSON = withText "name" (pure . ServerName)



data ServerVal
  = SAWTerm Term
