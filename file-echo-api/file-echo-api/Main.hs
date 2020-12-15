{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import qualified Data.Aeson as JSON
import           Data.ByteString ( ByteString )
import Data.Text (Text)
import qualified Options.Applicative as Opt

import qualified Argo as Argo
import Argo.DefaultMain ( customMain, userOptions )


import qualified FileEchoServer as FES

main :: IO ()
main = customMain parseServerOptions parseServerOptions parseServerOptions description getApp
  where
    getApp opts =
      Argo.mkApp (mkInitState $ userOptions opts) serverMethods

description :: String
description =
  "An RPC server for loading and printing files."

mkInitState :: ServerOptions -> (FilePath -> IO ByteString) -> IO FES.ServerState
mkInitState opts reader = FES.initialState (initialFile opts) reader

newtype ServerOptions = ServerOptions { initialFile :: Maybe FilePath }

parseServerOptions :: Opt.Parser ServerOptions
parseServerOptions = ServerOptions <$> filename
  where
    filename =
      Opt.optional $ Opt.strOption $
      Opt.long "file" <>
      Opt.metavar "FILENAME" <>
      Opt.help "Initial file to echo"

serverMethods :: [(Text, Argo.MethodType, JSON.Value -> Argo.Method FES.ServerState JSON.Value)]
serverMethods =
  [ ("load",           Argo.Command,   Argo.method FES.loadCmd)
  , ("clear",          Argo.Command,   Argo.method FES.clearCmd)
  , ("implode",        Argo.Query,     Argo.method FES.implodeCmd)
  , ("show",           Argo.Query,     Argo.method FES.showCmd)
  ]
