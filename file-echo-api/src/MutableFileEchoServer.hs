{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{- Like FileEchoServer but the underlying state uses mutability
   to update the loaded file contents. -}
module MutableFileEchoServer ( module MutableFileEchoServer ) where

import qualified Argo as Argo
import qualified Argo.Doc as Doc
import Control.Concurrent ( threadDelay )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.:?), (.=), (.!=) )
import Data.ByteString ( ByteString )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Scientific
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

instance Doc.DescribedMethod LoadParams () where
  parameterFieldDescription =
    [("file path",
      Doc.Paragraph [Doc.Text "The file to read into memory."])]

loadCmd :: LoadParams -> Argo.Command ServerState ()
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

instance Doc.DescribedMethod ClearParams () where
  parameterFieldDescription = []

clearCmd :: ClearParams -> Argo.Command ServerState ()
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

instance Doc.DescribedMethod ShowParams JSON.Value where
  parameterFieldDescription =
    [ ("start",
       Doc.Paragraph [Doc.Text "Start index (inclusive). If not provided, the substring is from the beginning of the file."])
    , ("end", Doc.Paragraph [Doc.Text "End index (exclusive). If not provided, the remainder of the file is returned."])
                              ]

  resultFieldDescription =
    [ ("value",
      Doc.Paragraph [ Doc.Text "The substring ranging from "
                    , Doc.Literal "start", Doc.Text " to ", Doc.Literal "end"
                    , Doc.Text "." ])
    ]


showCmd :: ShowParams -> Argo.Query ServerState JSON.Value
showCmd (ShowParams start end) = do
  appState <- Argo.getState
  (FileContents contents) <- liftIO $ readIORef $ fileContents appState
  let len = case end of
            Nothing -> length contents
            Just idx -> idx - start
  pure (JSON.object [ "value" .= JSON.String (T.pack $ take len $ drop start contents)])

------------------------------------------------------------------------
-- Destroy State Command
data DestroyStateParams =
  DestroyStateParams
  {
    stateToDestroy :: !Argo.StateID
  }

instance JSON.FromJSON DestroyStateParams where
  parseJSON =
    JSON.withObject "params for \"destroy state\"" $
    \o -> DestroyStateParams <$> o .: "state to destroy"

instance Doc.DescribedMethod DestroyStateParams () where
  parameterFieldDescription =
    [("state to destroy",
       Doc.Paragraph [Doc.Text "The state to destroy in the server (so it can be released from memory)."])
     ]

destroyState :: DestroyStateParams -> Argo.Notification ()
destroyState (DestroyStateParams stateID) = Argo.destroyState stateID


------------------------------------------------------------------------
-- Sleep Query

newtype SleepParams = SleepParams Int

instance JSON.FromJSON SleepParams where
  parseJSON =
    JSON.withObject "params for \"sleep\"" $
    \o -> SleepParams <$> o .: "microseconds"

instance Doc.DescribedMethod SleepParams JSON.Value where
  parameterFieldDescription =
    [("microseconds",
      Doc.Paragraph [Doc.Text "The duration to sleep in microseconds."])]

  resultFieldDescription =
    [ ("value",
      Doc.Paragraph [ Doc.Text "Duration in seconds sleep lasted."])
    ]

sleepQuery :: SleepParams -> Argo.Query ServerState JSON.Value
sleepQuery (SleepParams ms) = liftIO $ do
  t1 <- round `fmap` getPOSIXTime
  threadDelay ms
  t2 <- round `fmap` getPOSIXTime
  pure (JSON.object [ "value" .= (JSON.Number (scientific (t2 - t1) 0))])



------------------------------------------------------------------------
-- Interrupt All Threads Command
data InterruptAllThreadsParams = InterruptAllThreadsParams

instance JSON.FromJSON InterruptAllThreadsParams where
  parseJSON =
    JSON.withObject "params for \"interrupt all threads\"" $
    \_ -> pure InterruptAllThreadsParams

instance Doc.DescribedMethod InterruptAllThreadsParams () where
  parameterFieldDescription = []


interruptAllThreads :: InterruptAllThreadsParams -> Argo.Notification ()
interruptAllThreads _ = Argo.interruptAllThreads


------------------------------------------------------------------------
-- SlowClear Command

newtype SlowClear = SlowClear Int

instance JSON.FromJSON SlowClear where
  parseJSON =
    JSON.withObject "params for \"slow clear\"" $
    \o -> SlowClear <$> o .: "pause microseconds"

instance Doc.DescribedMethod SlowClear () where
  parameterFieldDescription =
    [("pause microseconds",
      Doc.Paragraph [Doc.Text "The duration to sleep in microseconds between each character being cleared."])]

slowClear :: SlowClear -> Argo.Command ServerState ()
slowClear (SlowClear ms) = do
  appState <- Argo.getState
  let go = do (FileContents contents) <- liftIO $ readIORef (fileContents appState)
              case contents of
                [] -> Argo.modifyState $ \s -> s { loadedFile = Nothing }
                (_:cs) -> do
                  liftIO $ writeIORef (fileContents appState) $ FileContents cs
                  liftIO $ threadDelay ms
                  go
  go
