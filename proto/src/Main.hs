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
import CryptolServer.ChangeDir
import CryptolServer.EvalExpr
import CryptolServer.LoadModule


main :: IO ()
main = realMain

realMain =
  do initSt <- initialState
     theApp <- mkApp initSt cryptolMethods
     serveStdIONS theApp
  where bogus = [("echo", Command (\r s p -> return (s, p)))]


cryptolMethods :: [(Text, Method ServerState)]
cryptolMethods =
  [ ("change directory", Command $ runCryptolServerCommand cd)
  , ("load module", Command $ runCryptolServerCommand loadModule)
  , ("evaluate expression", Command $ runCryptolServerCommand evalExpression)
  ]




parseParams v = (,) <$> JSON.parseJSON v <*> (JSON.withObject "state" (\o -> o .: "state") v)

data ServerHistory =
  HistLoadModule LoadModuleParams

instance JSON.FromJSON ServerHistory where
  parseJSON =
    JSON.withObject ("history entry") $
    \o -> do (cmd :: Text) <- o .: "command"
             case cmd of
               "load module" -> HistLoadModule <$> (o .: "params")
               _ -> empty




