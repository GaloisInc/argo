{-# LANGUAGE OverloadedStrings #-}
module FileEchoServer ( module FileEchoServer ) where

import qualified Argo as Argo
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Aeson as JSON
import           Data.Aeson ( (.:), (.:?), (.=), (.!=) )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified System.Directory as Dir
import           Control.Exception ( throwIO )


newtype FileContents = FileContents String

data ServerState = ServerState 
  { loadedFile :: Maybe FilePath
  -- ^ Loaded file (if any).
  , fileContents :: FileContents
  -- ^ Current file contents, or "" if one has not been loaded yet.
  }

initialState :: IO ServerState
initialState = pure $ ServerState Nothing (FileContents "")

newtype ServerErr = ServerErr String
newtype ServerRes a = ServerRes (Either ServerErr (a,FileContents))
newtype ServerCmd a = 
  ServerCmd (((FilePath -> IO ByteString), FileContents) -> IO (ServerRes a))


------------------------------------------------------------------------
-- Command Execution

runServerCmd :: ServerCmd a -> Argo.Method ServerState a
runServerCmd (ServerCmd cmd) =
    do s <- Argo.getState
       reader <- Argo.getFileReader
       out <- liftIO $ cmd (reader, fileContents s)
       case out of
         ServerRes (Left (ServerErr message)) ->
           Argo.raise $ Argo.makeJSONRPCException
            11000 "File Server exception"
            (Just (JSON.object ["error" .= message]))
         ServerRes (Right (x, newFileContents)) ->
           do Argo.setState (s { fileContents = newFileContents})
              return x


------------------------------------------------------------------------
-- Errors

fileNotFound :: FilePath -> Argo.JSONRPCException
fileNotFound fp =
  Argo.makeJSONRPCException
    20051 (T.pack ("File doesn't exist: " <> fp))
    (Just (JSON.object ["path" .= fp]))

------------------------------------------------------------------------
-- Load Command

data LoadParams = LoadParams FilePath

instance JSON.FromJSON LoadParams where
  parseJSON =
    JSON.withObject "params for \"load\"" $
    \o -> LoadParams <$> o .: "file path"

loadCmd :: LoadParams -> Argo.Method ServerState ()
loadCmd (LoadParams file) =
  do exists <- liftIO $ Dir.doesFileExist file
     if exists
     then do getFileContents <- Argo.getFileReader
             contents <- liftIO $ getFileContents file
             Argo.setState $ ServerState 
              { loadedFile = Just file
              , fileContents = FileContents $ Char8.unpack contents
              }
     else Argo.raise (fileNotFound file)


------------------------------------------------------------------------
-- Clear Command

data ClearParams = ClearParams

instance JSON.FromJSON ClearParams where
  parseJSON =
    JSON.withObject "params for \"show\"" $
    \_ -> pure ClearParams

clearCmd :: ClearParams -> Argo.Method ServerState ()
clearCmd _ =
  do Argo.setState $ ServerState
      { loadedFile = Nothing
      , fileContents = FileContents ""
      }

-- Substring ------------------------------------------------------------

data ShowParams = ShowParams
  { showStart :: Int
    -- ^ Inclusive start index in contents.
  , showEnd :: Maybe Int
  -- ^ Exclusive end index in contents.
  }

instance JSON.FromJSON ShowParams where
  parseJSON =
    JSON.withObject "params for \"show\"" $
    \o -> do start <- o .:? "start" .!= 0
             end <- o   .:? "end"
             pure $ ShowParams start end

showCmd :: ShowParams -> Argo.Method ServerState JSON.Value
showCmd (ShowParams start end) =
  do (FileContents contents) <-  fileContents <$> Argo.getState
     let len = case end of
                Nothing -> length contents
                Just idx -> idx - start 
     pure (JSON.object [ "value" .= (JSON.String $ T.pack $ take len $ drop start contents)])


------------------------------------------------------------------------
-- Implode Command

data ImplodeParams = ImplodeParams

instance JSON.FromJSON ImplodeParams where
  parseJSON =
    JSON.withObject "params for \"implode\"" $
    \_ -> pure ImplodeParams

implodeCmd :: ClearParams -> Argo.Method ServerState ()
implodeCmd _ = liftIO $ throwIO $ Argo.internalError
