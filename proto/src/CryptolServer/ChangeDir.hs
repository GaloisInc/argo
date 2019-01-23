{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.ChangeDir where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson as JSON
import qualified Data.Text as T
import System.Directory

import CryptolServer
import JSONRPC

cd :: CryptolServerCommand JSON.Value
cd =
  do (ChangeDirectoryParams newDir) <- params
     exists <- liftIO $ doesDirectoryExist newDir
     if exists
       then do liftIO $ setCurrentDirectory newDir
               return (toJSON ())
       else do rid <- getRequestID
               liftIO $ throwIO (dirNotFound rid newDir)

data ChangeDirectoryParams =
  ChangeDirectoryParams { newDirectory :: FilePath }

instance FromJSON ChangeDirectoryParams where
  parseJSON =
    withObject "params for \"change directory\"" $
    \o -> ChangeDirectoryParams <$> o .: "directory"

dirNotFound :: RequestID -> FilePath -> JSONRPCException
dirNotFound rid dir =
  JSONRPCException { errorCode = 3
                   , message = "Directory doesn't exist"
                   , errorData = Just (toJSON dir)
                   , errorID = Just rid
                   }
