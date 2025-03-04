{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import Data.ByteString (ByteString)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt

import qualified Argo as Argo
import qualified Argo.Doc as Doc
import Argo.DefaultMain ( customMain, userOptions )

import qualified MutableFileEchoServer as MFES
import qualified Paths_file_echo_api

main :: IO ()
main = customMain parseServerOptions parseServerOptions parseServerOptions parseServerOptions version description getApp
  where
    getApp opts =
      Argo.mkApp
        "mutable-file-echo-api"
        docs
        (Argo.defaultAppOpts Argo.MutableState)
        (mkInitState $ userOptions opts)
        serverMethods

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

-- Display the version number when the --version option is supplied.
version :: Opt.Parser (a -> a)
version = Opt.simpleVersioner (showVersion Paths_file_echo_api.version)

serverMethods :: [Argo.AppMethod MFES.ServerState]
serverMethods =
  [ Argo.command "load" (Doc.Paragraph [Doc.Text "Load a file from disk into memory."]) MFES.loadCmd
  , Argo.command "clear" (Doc.Paragraph [Doc.Text "Forget the loaded file."]) MFES.clearCmd
  , Argo.command "slow clear" (Doc.Paragraph [Doc.Text "Forgets the loaded file slowly (i.e., char by char)."]) MFES.slowClear
  , Argo.query "show" (Doc.Paragraph [Doc.Text "Show a substring of the file."]) MFES.showCmd
  , Argo.query "sleep query" (Doc.Paragraph [Doc.Text "Sleep for a specified number of microseconds."]) MFES.sleepQuery
  , Argo.notification "destroy state" (Doc.Paragraph [Doc.Text "Destroy a state."]) MFES.destroyState
  , Argo.notification "interrupt" (Doc.Paragraph [Doc.Text "Interrupt all threads in the server."]) MFES.interruptAllThreads
  ]
