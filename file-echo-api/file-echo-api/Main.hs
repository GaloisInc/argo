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

import qualified FileEchoServer as FES

main :: IO ()
main = customMain parseServerOptions parseServerOptions parseServerOptions parseServerOptions description getApp
  where
    getApp opts =
      Argo.mkDefaultApp "file-echo-api" docs (mkInitState $ userOptions opts) serverMethods

docs :: [Doc.Block]
docs =
  [ Doc.Paragraph [Doc.Text "A sample server that demonstrates filesystem caching."]
  , Doc.Section "Datatypes" [Doc.datatype @FES.Ignorable]
  ]

description :: String
description =
  "An RPC server for loading and printing files."

mkInitState :: ServerOptions -> (FilePath -> IO ByteString) -> IO FES.ServerState
mkInitState opts reader = FES.initialState (initialFile opts) reader

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

serverMethods :: [Argo.AppMethod FES.ServerState]
serverMethods =
  [ Argo.method "load" Argo.Command (Doc.Paragraph [Doc.Text "Load a file from disk into memory."]) FES.loadCmd
  , Argo.method "clear" Argo.Command (Doc.Paragraph [Doc.Text "Forget the loaded file."]) FES.clearCmd
  , Argo.method "prepend" Argo.Command (Doc.Paragraph [Doc.Text "Append a string to the left of the current contents."]) FES.prependCmd
  , Argo.method "drop" Argo.Command (Doc.Paragraph [Doc.Text "Drop from the left of the current contents."]) FES.dropCmd
  , Argo.method "implode" Argo.Query (Doc.Paragraph [Doc.Text "Throw an error immediately."]) FES.implodeCmd
  , Argo.method "show" Argo.Query (Doc.Paragraph [Doc.Text "Show a substring of the file."]) FES.showCmd
  , Argo.method "ignore" Argo.Query (Doc.Paragraph [Doc.Text "Ignore an ", Doc.Link (Doc.TypeDesc (typeRep (Proxy @FES.Ignorable))) "ignorable value", Doc.Text "."]) FES.ignoreCmd
  , Argo.method "pin state" Argo.Notification (Doc.Paragraph [Doc.Text "Pins a state in the server so it is available until unpinned."]) FES.pinState
  , Argo.method "unpin state" Argo.Notification (Doc.Paragraph [Doc.Text "Unpins a state in the server."]) FES.unpinState
  , Argo.method "unpin all states" Argo.Notification (Doc.Paragraph [Doc.Text "Unpin all states in the server."]) FES.unpinAllStates
  , Argo.method "set cache limit" Argo.Notification (Doc.Paragraph [Doc.Text "Unpin all states in the server."]) FES.setCacheLimit
  ]
