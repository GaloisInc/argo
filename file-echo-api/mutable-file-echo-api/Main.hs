{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import Data.ByteString (ByteString)
import Data.Typeable
import qualified Options.Applicative as Opt

import qualified Argo as Argo
import qualified Argo.Doc as Doc
import Argo.DefaultMain ( customMain, userOptions )

import qualified MutableFileEchoServer as MFES

main :: IO ()
main = customMain parseServerOptions parseServerOptions parseServerOptions parseServerOptions description getApp
  where
    getApp opts =
      Argo.mkDefaultApp "file-echo-api" docs (mkInitState $ userOptions opts) serverMethods

docs :: [Doc.Block]
docs =
  [ Doc.Paragraph [Doc.Text "A sample server that demonstrates filesystem caching with a mutable application state."]
  ]

description :: String
description =
  "An RPC server for loading and printing files."

mkInitState :: ServerOptions -> (FilePath -> IO ByteString) -> IO MFES.ServerState
mkInitState opts reader = MFES.initialState (initialFile opts) reader

newtype ServerOptions = ServerOptions { initialFile :: Maybe FilePath }

-- This function parses additional options used by this particular
-- application. The ordinary Argo options are still parsed, and these
-- are appended.
parseServerOptions :: Opt.Parser ServerOptions
parseServerOptions = ServerOptions <$> filename
  where
    filename =
      Opt.optional $ Opt.strOption $
      Opt.long "file" <>
      Opt.metavar "FILENAME" <>
      Opt.help "Initial file to echo"

serverMethods :: [Argo.AppMethod MFES.ServerState]
serverMethods =
  [ Argo.method "load" Argo.Command (Doc.Paragraph [Doc.Text "Load a file from disk into memory."]) MFES.loadCmd
  , Argo.method "clear" Argo.Command (Doc.Paragraph [Doc.Text "Forget the loaded file."]) MFES.clearCmd
  , Argo.method "show" Argo.Query (Doc.Paragraph [Doc.Text "Show a substring of the file."]) MFES.showCmd
  ]
