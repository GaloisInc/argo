{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.Names (visibleNames) where

import Control.Lens hiding ((.:), (.=))
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)

import Cryptol.Parser.Name (PName(..))
import Cryptol.ModuleSystem.Env (dynamicEnv, focusedEnv)
import Cryptol.ModuleSystem.Interface (IfaceDecl(..), IfaceDecls(..))
import Cryptol.ModuleSystem.Name (Name)
import Cryptol.ModuleSystem.NamingEnv (NamingEnv(..), lookupValNames, shadowing)
import Cryptol.Utils.PP (pp)

import Argo.JSONRPC

import CryptolServer

import Debug.Trace

visibleNames :: Method ServerState
visibleNames = query $ \() ->
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
         in NameInfo (show (pp n')) (show (pp ty)) docs
  | n <- lookupValNames n' rnEnv
  ]

data NameInfo =
  NameInfo
  { name :: String
  , typeSig :: String
  , docs :: Maybe String
  }

instance JSON.ToJSON NameInfo where
  toJSON (NameInfo x ty doc) =
    JSON.object $ ["name" .= x, "type" .= ty] ++
                  maybe [] (\d -> ["documentation" .= d]) doc
