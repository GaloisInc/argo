{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Typeable

import qualified Argo as Argo
import qualified Argo.Doc as Doc
import Argo.DefaultMain (defaultMain)


import qualified FileEchoServer as FES

main :: IO ()
main =
  do theApp <- Argo.mkApp "File echo server" docs mkInitState serverMethods
     defaultMain description theApp

docs :: [Doc.Block]
docs =
  [ Doc.Paragraph [Doc.Text "A sample server that demonstrates filesystem caching."]
  , Doc.Section "Datatypes" [Doc.datatype @FES.Ignorable]
  ]

description :: String
description =
  "An RPC server for loading and printing files."

mkInitState :: (FilePath -> IO ByteString) -> IO FES.ServerState 
mkInitState = const FES.initialState

serverMethods :: [Argo.AppMethod FES.ServerState]
serverMethods =
  [ Argo.method "load" Argo.Command (Doc.Paragraph [Doc.Text "Load a file from disk into memory."]) FES.loadCmd
  , Argo.method "clear" Argo.Command (Doc.Paragraph [Doc.Text "Forget the loaded file."]) FES.clearCmd
  , Argo.method "implode" Argo.Query (Doc.Paragraph [Doc.Text "Throw an error immediately."]) FES.implodeCmd
  , Argo.method "show" Argo.Query (Doc.Paragraph [Doc.Text "Show a substring of the file."]) FES.showCmd
  , Argo.method "ignore" Argo.Query (Doc.Paragraph [Doc.Text "Ignore an ", Doc.Link (Doc.TypeDesc (typeRep (Proxy @FES.Ignorable))) "ignorable value", Doc.Text "."]) FES.ignoreCmd
  ]
