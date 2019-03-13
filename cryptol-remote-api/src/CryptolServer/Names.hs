{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.Names (visibleNames) where

import Control.Lens hiding ((.:), (.=))
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T

import Cryptol.Parser.Name (PName(..))
import Cryptol.Parser.Selector (ppSelector)
import Cryptol.ModuleSystem.Env (dynamicEnv, focusedEnv)
import Cryptol.ModuleSystem.Interface (IfaceDecl(..), IfaceDecls(..))
import Cryptol.ModuleSystem.Name (Name)
import Cryptol.ModuleSystem.NamingEnv (NamingEnv(..), lookupValNames, shadowing)
import Cryptol.TypeCheck.PP (NameMap, emptyNameMap, ppWithNames)
import Cryptol.TypeCheck.Type (Kind(..), PC(..), TC(..), TCon(..), TFun(..), TParam(..), Type(..), Schema(..), addTNames, kindOf)
import Cryptol.Utils.PP (pp)

import Argo.JSONRPC

import CryptolServer
import CryptolServer.Data.Type

import Debug.Trace

visibleNames :: CryptolServerQuery JSON.Value
visibleNames =
  do me <- view moduleEnv <$> getState
     let (dyDecls,dyNames,dyDisp) = dynamicEnv me
     let (fParams,fDecls,fNames,fDisp) = focusedEnv me
     let inScope = Map.keys (neExprs $ dyNames `shadowing` fNames)
     return $ JSON.toJSON (concatMap (getInfo fNames (ifDecls fDecls)) inScope)

getInfo :: NamingEnv -> Map Name IfaceDecl -> PName -> [NameInfo]
getInfo rnEnv info n' =
  [ case Map.lookup n info of
       Nothing -> error $ "Name not found, but should have been: " ++ show n
       Just i ->
         let ty = ifDeclSig i
             docs = ifDeclDoc i
         in NameInfo (show (pp n')) (show (pp ty)) ty docs
  | n <- lookupValNames n' rnEnv
  ]

data NameInfo =
  NameInfo
  { name :: String
  , typeSig :: String
  , schema :: Schema
  , docs :: Maybe String
  }

instance JSON.ToJSON NameInfo where
  toJSON (NameInfo x tyS ty doc) =
    JSON.object $
    [ "name" .= x
    , "type string" .= tyS
    , "type" .= JSON.toJSON (JSONSchema ty)
    ] ++
    maybe [] (\d -> ["documentation" .= d]) doc


