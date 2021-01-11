{-# LANGUAGE OverloadedStrings #-}
module FileEchoServer ( module FileEchoServer ) where

import qualified Argo as Argo
import qualified Argo.Doc as Doc
import Control.Exception ( throwIO )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.:?), (.=), (.!=) )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
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

instance Doc.DescribedParams PrependParams where
  parameterFieldDescription =
    [("content",
      Doc.Paragraph [Doc.Text "The string to append to the left of the current file content on the server."])]

prependCmd :: PrependParams -> Argo.Method ServerState ()
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

instance Doc.DescribedParams DropParams where
  parameterFieldDescription =
    [("count",
      Doc.Paragraph [Doc.Text "The number of characters to drop from the left of the current file content on the server."])]

dropCmd :: DropParams -> Argo.Method ServerState ()
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

instance Doc.DescribedParams ClearParams where
  parameterFieldDescription = []

clearCmd :: ClearParams -> Argo.Method ServerState ()
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

instance Doc.DescribedParams ShowParams where
  parameterFieldDescription =
    [ ("start",
       Doc.Paragraph [Doc.Text "Start index (inclusive). If not provided, the substring is from the beginning of the file."])
    , ("end", Doc.Paragraph [Doc.Text "End index (exclusive). If not provided, the remainder of the file is returned."])
                              ]


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

instance Doc.DescribedParams ImplodeParams where
  parameterFieldDescription = []


implodeCmd :: ClearParams -> Argo.Method ServerState ()
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

instance Doc.DescribedParams IgnoreParams where
  parameterFieldDescription =
    [("to be ignored",
      Doc.Paragraph [Doc.Text "The value to be ignored goes here."])]

ignoreCmd :: IgnoreParams -> Argo.Method ServerState ()
ignoreCmd _ = pure ()



------------------------------------------------------------------------
-- Pin State Command
data PinStateParams =
  PinStateParams
  {
    stateToPin :: !Argo.StateID
  }

instance JSON.FromJSON PinStateParams where
  parseJSON =
    JSON.withObject "params for \"pin state\"" $
    \o -> PinStateParams <$> o .: "state to pin"

instance Doc.DescribedParams PinStateParams where
  parameterFieldDescription = 
    [("state to pin",
       Doc.Paragraph [Doc.Text "The state to pin in the server so it is available until unpinned."])
     ]

pinState :: PinStateParams -> Argo.Method ServerState ()
pinState (PinStateParams stateID) = Argo.pinState stateID


------------------------------------------------------------------------
-- Unpin State Command
data UnpinStateParams =
  UnpinStateParams
  {
    stateToUnpin :: !Argo.StateID
  }

instance JSON.FromJSON UnpinStateParams where
  parseJSON =
    JSON.withObject "params for \"unpin state\"" $
    \o -> UnpinStateParams <$> o .: "state to unpin"

instance Doc.DescribedParams UnpinStateParams where
  parameterFieldDescription = 
    [("state to unpin",
       Doc.Paragraph [Doc.Text "The state to unpin in the server (so it can be released from memory)."])
     ]

unpinState :: UnpinStateParams -> Argo.Method ServerState ()
unpinState (UnpinStateParams stateID) = Argo.unpinState stateID



------------------------------------------------------------------------
-- Unpin All States Command
data UnpinAllStatesParams = UnpinAllStatesParams

instance JSON.FromJSON UnpinAllStatesParams where
  parseJSON =
    JSON.withObject "params for \"unpin all states\"" $
    \_ -> pure UnpinAllStatesParams

instance Doc.DescribedParams UnpinAllStatesParams where
  parameterFieldDescription = []


unpinAllStates :: UnpinAllStatesParams -> Argo.Method ServerState ()
unpinAllStates _ = Argo.unpinAllStates

------------------------------------------------------------------------
-- Set Cache Limit Command
data SetCacheLimitParams =
  SetCacheLimitParams
  {
    desiredCacheLimit :: !Int
  }

instance JSON.FromJSON SetCacheLimitParams where
  parseJSON =
    JSON.withObject "params for \"set cache limit\"" $
    \o -> SetCacheLimitParams <$> o .: "cache limit"

instance Doc.DescribedParams SetCacheLimitParams where
  parameterFieldDescription = 
    [("cache limit",
      Doc.Paragraph [Doc.Text "Limit how many temporarily cached states can accumulate."])]

setCacheLimit :: SetCacheLimitParams -> Argo.Method ServerState ()
setCacheLimit (SetCacheLimitParams n) = Argo.setCacheLimit n

