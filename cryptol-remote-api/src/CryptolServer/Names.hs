{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.Names (visibleNames) where

import Control.Lens hiding ((.=))
import qualified Data.Aeson as JSON
import Data.Aeson ((.=))
import qualified Data.Map as Map
import Data.Map (Map)

import Cryptol.Parser.Name (PName(..))
import Cryptol.ModuleSystem.Env (dynamicEnv, focusedEnv)
import Cryptol.ModuleSystem.Interface (IfaceDecl(..), IfaceDecls(..))
import Cryptol.ModuleSystem.Name (Name)
import Cryptol.ModuleSystem.NamingEnv (NamingEnv(..), lookupValNames, shadowing)
import Cryptol.TypeCheck.Type (Schema(..))
import Cryptol.Utils.PP (pp)

import Argo

import CryptolServer
import CryptolServer.Data.Type


visibleNames :: JSON.Value -> Method ServerState [NameInfo]
visibleNames _ =
  do me <- view moduleEnv <$> getState
     let (_dyDecls, dyNames, _dyDisp) = dynamicEnv me
     let (_fParams, fDecls, fNames, _fDisp) = focusedEnv me
     let inScope = Map.keys (neExprs $ dyNames `shadowing` fNames)
     return $ concatMap (getInfo fNames (ifDecls fDecls)) inScope

getInfo :: NamingEnv -> Map Name IfaceDecl -> PName -> [NameInfo]
getInfo rnEnv info n' =
  [ case Map.lookup n info of
       Nothing -> error $ "Name not found, but should have been: " ++ show n
       Just i ->
         let ty = ifDeclSig i
             nameDocs = ifDeclDoc i
         in NameInfo (show (pp n')) (show (pp ty)) ty nameDocs
  | n <- lookupValNames n' rnEnv
  ]

data NameInfo =
  NameInfo
  { name     :: String
  , typeSig  :: String
  , schema   :: Schema
  , nameDocs :: Maybe String
  }

instance JSON.ToJSON NameInfo where
  toJSON (NameInfo{name, typeSig, schema, nameDocs}) =
    JSON.object $
    [ "name" .= name
    , "type string" .= typeSig
    , "type" .= JSON.toJSON (JSONSchema schema)
    ] ++
    maybe [] (\d -> ["documentation" .= d]) nameDocs
