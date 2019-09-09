{-# LANGUAGE OverloadedStrings #-}
module SAWServer.SaveTerm where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.=), (.:))
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T

import Cryptol.Eval (EvalOpts(..), defaultPPOpts)
import Cryptol.ModuleSystem (ModuleRes)
import Cryptol.ModuleSystem.Base (genInferInput, getPrimMap, noPat, rename)
import Cryptol.ModuleSystem.Env (ModuleEnv)
import Cryptol.ModuleSystem.Interface (noIfaceParams)
import Cryptol.ModuleSystem.Monad (ModuleM, interactive, runModuleM, setNameSeeds, setSupply, typeCheckWarnings, typeCheckingFailed)
import qualified Cryptol.ModuleSystem.Renamer as MR
import Cryptol.Parser.Position (emptyRange, getLoc)
import Cryptol.TypeCheck (tcExpr)
import Cryptol.TypeCheck.Monad (InferOutput(..), inpVars, inpTSyns)
import Cryptol.Utils.Ident (interactiveName)
import Cryptol.Utils.Logger (quietLogger)
import Cryptol.Utils.PP
import SAWScript.Value (biSharedContext)
import Verifier.SAW.CryptolEnv
import Verifier.SAW.TypedTerm(TypedTerm(..))


import Argo

import CryptolServer.Data.Expression
import SAWServer
import SAWServer.CryptolExpression
import SAWServer.Exceptions
import SAWServer.OK


saveTerm :: SaveTermParams -> Method SAWState OK
saveTerm (SaveTermParams name e) =
  do setServerVal name =<< getTypedTerm e
     ok




data SaveTermParams =
  SaveTermParams
    { termName :: ServerName
    , termExpr :: Expression
    }

instance FromJSON SaveTermParams where
  parseJSON =
    withObject "parameters for saving a term" $ \o ->
    SaveTermParams <$> o .: "name"
                   <*> o .: "expression"
