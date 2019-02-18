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
import System.Directory (doesDirectoryExist, setCurrentDirectory)

import JSONRPC


import Debug.Trace

import CryptolServer
import CryptolServer.Call
import CryptolServer.ChangeDir
import CryptolServer.EvalExpr
import CryptolServer.LoadModule

import HistoryWrapper


main :: IO ()
main = realMain

realMain :: IO ()
realMain =
  do initSt <- initialState
     theApp <- mkApp HistoryWrapper (historyWrapper cryptolMethods initSt)
     serveStdIONS theApp


cryptolMethods :: [(Text, Method ServerState)]
cryptolMethods =
  [ ("change directory",        Command $ runCryptolServerCommand cd)
  , ("load module",             Command $ runCryptolServerCommand loadModule)
  , ("evaluate expression",     Command $ runCryptolServerCommand evalExpression)
  , ("call",                    Command $ runCryptolServerCommand call)
  ]
