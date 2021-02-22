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
      Argo.mkApp
        "file-echo-api"
        docs
        (Argo.defaultAppOpts Argo.PureState)
        (mkInitState $ userOptions opts)
        serverMethods

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
  [ Argo.command "load" (Doc.Paragraph [Doc.Text "Load a file from disk into memory."]) FES.loadCmd
  , Argo.command "clear" (Doc.Paragraph [Doc.Text "Forget the loaded file."]) FES.clearCmd
  , Argo.command "prepend" (Doc.Paragraph [Doc.Text "Append a string to the left of the current contents."]) FES.prependCmd
  , Argo.command "drop" (Doc.Paragraph [Doc.Text "Drop from the left of the current contents."]) FES.dropCmd
  , Argo.command "implode" (Doc.Paragraph [Doc.Text "Throw an error immediately."]) FES.implodeCmd
  , Argo.command "show" (Doc.Paragraph [Doc.Text "Show a substring of the file."]) FES.showCmd
  , Argo.command "ignore" (Doc.Paragraph [Doc.Text "Ignore an ", Doc.Link (Doc.TypeDesc (typeRep (Proxy @FES.Ignorable))) "ignorable value", Doc.Text "."]) FES.ignoreCmd
  , Argo.notification "destroy state" (Doc.Paragraph [Doc.Text "Destroy a state in the server."]) FES.destroyState
  , Argo.notification "destroy all states" (Doc.Paragraph [Doc.Text "Destroy all states in the server."]) FES.destroyAllStates
  ]
