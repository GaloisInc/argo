{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module FileEchoServer ( module FileEchoServer ) where

import qualified Argo as Argo
import qualified Argo.Doc as Doc
import Control.Concurrent ( threadDelay )
import Control.Exception ( throwIO )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.:?), (.=), (.!=) )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Scientific (scientific)
import qualified System.Directory as Dir



newtype FileContents = FileContents String

data ServerState = ServerState
  { loadedFile :: Maybe FilePath
  -- ^ Loaded file (if any).
  , fileContents :: FileContents
  -- ^ Current file contents, or "" if one has not been loaded yet.
  }

initialState ::
  Maybe FilePath ->
  (FilePath -> IO ByteString) ->
  IO ServerState
initialState Nothing _reader =
  pure $ ServerState Nothing (FileContents "")
initialState (Just path) reader =
  do contents <- FileContents . Char8.unpack <$> reader path
     pure $ ServerState (Just path) contents

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
             Argo.setState $ ServerState
              { loadedFile = Just file
              , fileContents = FileContents $ Char8.unpack contents
              }
     else Argo.raise (fileNotFound file)


------------------------------------------------------------------------
-- Prepend Command

data PrependParams = PrependParams String

instance JSON.FromJSON PrependParams where
  parseJSON =
    JSON.withObject "params for \"prepend\"" $
    \o -> PrependParams <$> o .: "content"

instance Doc.DescribedMethod PrependParams () where
  parameterFieldDescription =
    [("content",
      Doc.Paragraph [Doc.Text "The string to append to the left of the current file content on the server."])]

prependCmd :: PrependParams -> Argo.Command ServerState ()
prependCmd (PrependParams str) =
  do (FileContents contents) <-  fileContents <$> Argo.getState
     Argo.setState $ ServerState
      { loadedFile = Nothing
      , fileContents = FileContents $ str ++ contents
      }

------------------------------------------------------------------------
-- Drop Command

data DropParams = DropParams Int

instance JSON.FromJSON DropParams where
  parseJSON =
    JSON.withObject "params for \"drop\"" $
    \o -> DropParams <$> o .: "count"

instance Doc.DescribedMethod DropParams () where
  parameterFieldDescription =
    [("count",
      Doc.Paragraph [Doc.Text "The number of characters to drop from the left of the current file content on the server."])]

dropCmd :: DropParams -> Argo.Command ServerState ()
dropCmd (DropParams n) =
  do (FileContents contents) <-  fileContents <$> Argo.getState
     Argo.setState $ ServerState
      { loadedFile = Nothing
      , fileContents = FileContents $ drop n contents
      }

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
clearCmd _ =
  do Argo.setState $ ServerState
      { loadedFile = Nothing
      , fileContents = FileContents ""
      }

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
showCmd (ShowParams start end) =
  do (FileContents contents) <-  fileContents <$> Argo.getState
     let len = case end of
                Nothing -> length contents
                Just idx -> idx - start
     pure (JSON.object [ "value" .= (JSON.String $ T.pack $ take len $ drop start contents)])


------------------------------------------------------------------------
-- Implode Query

data ImplodeParams = ImplodeParams

instance JSON.FromJSON ImplodeParams where
  parseJSON =
    JSON.withObject "params for \"implode\"" $
    \_ -> pure ImplodeParams

instance Doc.DescribedMethod ImplodeParams () where
  parameterFieldDescription = []


implodeCmd :: ClearParams -> Argo.Query ServerState ()
implodeCmd _ = liftIO $ throwIO Argo.internalError

----------------------------------------------------------------------

data Ignorable =
  ThisDatatype | ExistsTo | DemonstrateDocs

instance JSON.FromJSON Ignorable where
  parseJSON (JSON.Bool True) = pure ThisDatatype
  parseJSON (JSON.Bool False) = pure ExistsTo
  parseJSON JSON.Null = pure DemonstrateDocs
  parseJSON _ = fail "Unknown value"

instance Doc.Described Ignorable where
  typeName = "Ignorable data"
  description =
    [ Doc.Paragraph [Doc.Text "Data to be ignored can take one of three forms:"]
    , Doc.DescriptionList
        [ (pure $ Doc.Literal "true",
           Doc.Paragraph [Doc.Text "The first ignorable value"])
        , (pure $ Doc.Literal "false",
           Doc.Paragraph [Doc.Text "The second ignorable value"])
        , (pure $ Doc.Literal "null",
           Doc.Paragraph [Doc.Text "The ultimate ignorable value, neither true nor false"])
        ]
    , Doc.Paragraph [Doc.Text "Nothing else may be ignored."]
    ]


------------------------------------------------------------------------
-- Ignore Command
data IgnoreParams = IgnoreParams !Ignorable

instance JSON.FromJSON IgnoreParams where
  parseJSON =
    JSON.withObject "params for \"ignore\"" $
      \o -> IgnoreParams <$> o .: "to be ignored"

instance Doc.DescribedMethod IgnoreParams () where
  parameterFieldDescription =
    [("to be ignored",
      Doc.Paragraph [Doc.Text "The value to be ignored goes here."])]

ignoreCmd :: IgnoreParams -> Argo.Query ServerState ()
ignoreCmd _ = pure ()


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
-- Destroy All States Command
data DestroyAllStatesParams = DestroyAllStatesParams

instance JSON.FromJSON DestroyAllStatesParams where
  parseJSON =
    JSON.withObject "params for \"destroy all states\"" $
    \_ -> pure DestroyAllStatesParams

instance Doc.DescribedMethod DestroyAllStatesParams () where
  parameterFieldDescription = []


destroyAllStates :: DestroyAllStatesParams -> Argo.Notification ()
destroyAllStates _ = Argo.destroyAllStates


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
  let go = do (FileContents contents) <- fileContents <$> Argo.getState
              case contents of
                [] -> Argo.modifyState $ \s -> s { loadedFile = Nothing }
                (_:cs) -> do
                  Argo.modifyState $ \s -> s { fileContents = FileContents cs }
                  liftIO $ threadDelay ms
                  go
  go
