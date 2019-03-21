{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.Sat where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson ((.=), (.:), FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.IORef
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T

import Cryptol.Eval.Value (Value)
import Cryptol.ModuleSystem (ModuleCmd, ModuleEnv, checkExpr, evalExpr, getPrimMap, loadModuleByPath, loadModuleByName)
import Cryptol.ModuleSystem.Env (DynamicEnv(..), initialModuleEnv, meDynEnv, meSolverConfig)
import Cryptol.Symbolic (ProverCommand(..), ProverResult(..), QueryType(..), SatNum(..), proverNames, satProve)
import Cryptol.TypeCheck.AST (Expr, Type)
import Cryptol.TypeCheck.Solve (defaultReplExpr)
import qualified Cryptol.TypeCheck.Solver.SMT as SMT
import Cryptol.Utils.PP (pp)

import Argo.JSONRPC
import CryptolServer
import CryptolServer.Exceptions (evalPolyErr, proverError)
import CryptolServer.Data.Expression
import CryptolServer.Data.Type

sat (ProveSatParams (Prover name) jsonExpr num) =
  do e <- getExpr jsonExpr
     (expr, ty, schema) <- runModuleCmd (checkExpr e)
     -- TODO validEvalContext expr, ty, schema
     me <- view moduleEnv <$> getState
     let decls = deDecls (meDynEnv me)
     let cfg = meSolverConfig me
     perhapsDef <- liftIO $ SMT.withSolver cfg (\s -> defaultReplExpr s ty schema)
     case perhapsDef of
       Nothing ->
         raise (evalPolyErr (JSONSchema schema))
       Just (tys, checked) ->
         do timing <- liftIO $ newIORef 0
            let cmd =
                  ProverCommand
                  { pcQueryType    = SatQuery num
                  , pcProverName   = name
                  , pcVerbose      = True -- verbose
                  , pcProverStats  = timing
                  , pcExtraDecls   = decls
                  , pcSmtFile      = Nothing -- mfile
                  , pcExpr         = checked
                  , pcSchema       = schema
                  }
            (firstProver, res) <- runModuleCmd $ satProve cmd
            stats <- liftIO (readIORef timing)
            case res of
              ProverError msg -> raise (proverError msg)
              EmptyResult -> error "got empty result for online prover!"
              ThmResult ts -> pure Unsatisfiable
              AllSatResult results ->
                Satisfied <$> traverse satResult results

  where
    satResult :: [(Type, Expr, Value)] -> Method ServerState [(JSONType, Expression)]
    satResult es = traverse result es

    result (t, _, v) =
      do prims <- runModuleCmd getPrimMap
         e <- observe $ readBack prims t v
         return (JSONType mempty t, e)

data SatResult = Unsatisfiable | Satisfied [[(JSONType, Expression)]]

instance ToJSON SatResult where
  toJSON Unsatisfiable = JSON.object ["result" .= ("unsatisfiable" :: Text)]
  toJSON (Satisfied xs) =
    JSON.object [ "result" .= ("satisfied" :: Text)
                , "model" .=
                  [ [ JSON.object [ "type" .= t
                                  , "expr" .= e
                                  ]
                    | (t, e) <- res
                    ]
                  | res <- xs
                  ]
                ]


newtype Prover = Prover { proverName :: String }

instance FromJSON Prover where
  parseJSON =
    JSON.withText "prover name" $
    \txt ->
      let str = T.unpack txt
      in if str `elem` proverNames
           then return (Prover str)
           else empty


data ProveSatParams =
  ProveSatParams
    { prover :: Prover
    , expression :: Expression
    , numResults :: SatNum
    }

instance FromJSON ProveSatParams where
  parseJSON =
    JSON.withObject "sat or prove parameters" $
    \o -> ProveSatParams <$> o .: "prover"
                         <*> o .: "expression"
                         <*> (o .: "result count" >>= num)
    where
      num v = ((JSON.withText "all" $
               \t -> if t == "all" then pure AllSat else empty) v) <|>
              ((JSON.withScientific "count" $
                \s ->
                  case floatingOrInteger s of
                    Left float -> empty
                    Right int -> pure (SomeSat int)) v)
