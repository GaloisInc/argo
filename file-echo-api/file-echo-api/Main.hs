{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import qualified Data.Aeson as JSON
import           Data.ByteString ( ByteString )
import Data.Text (Text)

import qualified Argo as Argo
import Argo.DefaultMain ( defaultMain )


import qualified FileEchoServer as FES

main :: IO ()
main =
  do theApp <- Argo.mkApp mkInitState serverMethods
     defaultMain description theApp

description :: String
description =
  "An RPC server for loading and printing files."

mkInitState :: (FilePath -> IO ByteString) -> IO FES.ServerState 
mkInitState = const $ FES.initialState

serverMethods :: [(Text, Argo.MethodType, JSON.Value -> Argo.Method FES.ServerState JSON.Value)]
serverMethods =
  [ ("load",           Argo.Command,   Argo.method FES.loadCmd)
  , ("clear",          Argo.Command,   Argo.method FES.clearCmd)
  , ("implode",        Argo.Query,     Argo.method FES.implodeCmd)
  , ("show",           Argo.Query,     Argo.method FES.showCmd)
  ]
