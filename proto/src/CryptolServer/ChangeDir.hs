{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.ChangeDir (cd) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson as JSON
import qualified Data.Text as T
import System.Directory

import CryptolServer
import JSONRPC

cd :: CryptolServerCommand JSON.Value
cd =
  do ChangeDirectoryParams newDir <- params
     exists <- liftIO $ doesDirectoryExist newDir
     if exists
       then do liftIO $ setCurrentDirectory newDir
               return (toJSON ())
       else raise (dirNotFound newDir)

data ChangeDirectoryParams =
  ChangeDirectoryParams { newDirectory :: FilePath }

instance FromJSON ChangeDirectoryParams where
  parseJSON =
    withObject "params for \"change directory\"" $
    \o -> ChangeDirectoryParams <$> o .: "directory"

dirNotFound :: FilePath -> CryptolServerException
dirNotFound dir rid =
  JSONRPCException { errorCode = 3
                   , message = "Directory doesn't exist"
                   , errorData = Just (toJSON dir)
                   , errorID = Just rid
                   }
