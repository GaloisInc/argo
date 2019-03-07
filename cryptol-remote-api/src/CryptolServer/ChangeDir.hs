{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.ChangeDir (cd) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson as JSON
import qualified Data.Text as T
import System.Directory

import CryptolServer
import Argo.JSONRPC

cd :: Method ServerState
cd = command $ \(ChangeDirectoryParams newDir) ->
  do exists <- liftIO $ doesDirectoryExist newDir
     if exists
       then liftIO $ setCurrentDirectory newDir
       else raise (dirNotFound newDir)

data ChangeDirectoryParams =
  ChangeDirectoryParams { newDirectory :: FilePath }

instance FromJSON ChangeDirectoryParams where
  parseJSON =
    withObject "params for \"change directory\"" $
    \o -> ChangeDirectoryParams <$> o .: "directory"

dirNotFound :: FilePath -> RequestID -> JSONRPCException
dirNotFound dir rid =
  JSONRPCException { errorCode = 3
                   , message = "Directory doesn't exist"
                   , errorData = Just (toJSON dir)
                   , errorID = Just rid
                   }
