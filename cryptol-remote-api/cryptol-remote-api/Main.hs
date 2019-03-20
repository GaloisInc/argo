{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as JSON
import Data.Text (Text)
import qualified Options.Applicative as Opt
import System.Directory (doesDirectoryExist, setCurrentDirectory)
import System.IO (stdout)

import Argo
import Argo.Socket


import CryptolServer
import CryptolServer.Call
import CryptolServer.ChangeDir
import CryptolServer.EvalExpr
import CryptolServer.LoadModule
import CryptolServer.Names
import CryptolServer.TypeCheck

import Argo.HistoryWrapper
import Argo.CacheTree

main :: IO ()
main =
  do opts <- Opt.execParser options
     realMain opts


data Options =
  Options
    { transportOpt :: TransportOpt
    }

newtype Port = Port String

data TransportOpt = StdIONetstring | SocketNetstring Port

options :: Opt.ParserInfo Options
options = Opt.info (Options <$> transport) (Opt.fullDesc)

transport :: Opt.Parser TransportOpt
transport = socket <|> stdio
  where
    socket = SocketNetstring . Port <$> Opt.strOption (Opt.long "socket" <> Opt.metavar "PORT" )
    stdio = Opt.flag StdIONetstring StdIONetstring (Opt.long "stdio" <> Opt.help "Use netstrings over stdio")


realMain :: Options -> IO ()
realMain opts =
  do initSt <- initialState
     cache  <- newCache initSt
     theApp <- mkApp (HistoryWrapper cache) (historyWrapper validateServerState cryptolMethods)
     case transportOpt opts of
       StdIONetstring -> serveStdIONS theApp
       SocketNetstring (Port p) -> serveSocket (Just stdout) "127.0.0.1" p theApp


cryptolMethods :: [(Text, MethodType, JSON.Value -> Method ServerState JSON.Value)]
cryptolMethods =
  [ ("change directory",    Command, method cd)
  , ("load module",         Command, method loadModule)
  , ("evaluate expression", Query,   method evalExpression)
  , ("call",                Query,   method call)
  , ("visible names",       Query,   method visibleNames)
  , ("check type",          Query,   method checkType)
  ]
