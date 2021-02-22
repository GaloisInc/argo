{-# LANGUAGE OverloadedStrings #-}
{- Like FileEchoServer but the underlying state uses mutability
   to update the loaded file contents. -}
module MutableFileEchoServer ( module MutableFileEchoServer ) where

import qualified Argo as Argo
import qualified Argo.Doc as Doc
import Control.Exception ( throwIO )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.:?), (.=), (.!=) )
import Data.ByteString ( ByteString )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified System.Directory as Dir



newtype FileContents = FileContents String

data ServerState = ServerState
  { loadedFile :: Maybe FilePath
  -- ^ Loaded file (if any).
  , fileContents :: IORef FileContents
  -- ^ Current file contents, or "" if one has not been loaded yet.
  }

initialState ::
  Maybe FilePath ->
  (FilePath -> IO ByteString) ->
  IO ServerState
initialState Nothing _reader = do
  contentRef <- newIORef (FileContents "")
  pure $ ServerState Nothing contentRef
initialState (Just path) reader = do 
  contents <- FileContents . Char8.unpack <$> reader path
  contentRef <- newIORef contents
  pure $ ServerState (Just path) contentRef

newtype ServerErr = ServerErr String
newtype ServerRes a = ServerRes (Either ServerErr (a, FileContents))
newtype ServerCmd a =
  ServerCmd ((FilePath -> IO ByteString, FileContents) -> IO (ServerRes a))


------------------------------------------------------------------------
-- Command Execution

runServerCmd :: ServerCmd a -> Argo.Method ServerState a
runServerCmd (ServerCmd cmd) = do 
  contentRef <-  fileContents <$> Argo.getState
  contents <- liftIO $ readIORef contentRef
  reader <- Argo.getFileReader
  out <- liftIO $ cmd (reader, contents)
  case out of
    ServerRes (Left (ServerErr message)) ->
      Argo.raise $ Argo.makeJSONRPCException
       11000 "File Server exception"
       (Just (JSON.object ["error" .= message]))
    ServerRes (Right (x, newFileContents)) -> do
      liftIO $ writeIORef contentRef newFileContents
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

instance Doc.DescribedParams LoadParams where
  parameterFieldDescription =
    [("file path",
      Doc.Paragraph [Doc.Text "The file to read into memory."])]

loadCmd :: LoadParams -> Argo.Method ServerState ()
loadCmd (LoadParams file) =
  do exists <- liftIO $ Dir.doesFileExist file
     if exists
     then do getFileContents <- Argo.getFileReader
             contents <- liftIO $ getFileContents file
             appState <- Argo.getState
             liftIO $ writeIORef (fileContents appState) $ FileContents $ Char8.unpack contents
             Argo.setState $ appState { loadedFile = Just file }
     else Argo.raise (fileNotFound file)


------------------------------------------------------------------------
-- Clear Command

data ClearParams = ClearParams

instance JSON.FromJSON ClearParams where
  parseJSON =
    JSON.withObject "params for \"show\"" $
    \_ -> pure ClearParams

instance Doc.DescribedParams ClearParams where
  parameterFieldDescription = []

clearCmd :: ClearParams -> Argo.Method ServerState ()
clearCmd _ = do
  appState <- Argo.getState
  liftIO $ writeIORef (fileContents appState) $ FileContents ""
  Argo.setState $ appState { loadedFile = Nothing }

------------------------------------------------------------------------
-- Show Command

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

instance Doc.DescribedParams ShowParams where
  parameterFieldDescription =
    [ ("start",
       Doc.Paragraph [Doc.Text "Start index (inclusive). If not provided, the substring is from the beginning of the file."])
    , ("end", Doc.Paragraph [Doc.Text "End index (exclusive). If not provided, the remainder of the file is returned."])
                              ]


showCmd :: ShowParams -> Argo.Method ServerState JSON.Value
showCmd (ShowParams start end) = do
  appState <- Argo.getState
  (FileContents contents) <- liftIO $ readIORef $ fileContents appState
  let len = case end of
            Nothing -> length contents
            Just idx -> idx - start
  pure (JSON.object [ "value" .= JSON.String (T.pack $ take len $ drop start contents)])

