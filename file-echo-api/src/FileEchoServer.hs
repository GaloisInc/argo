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
        [ (Doc.Literal "true",
           Doc.Paragraph [Doc.Text "The first ignorable value"])
        , (Doc.Literal "false",
           Doc.Paragraph [Doc.Text "The second ignorable value"])
        , (Doc.Literal "null",
           Doc.Paragraph [Doc.Text "The ultimate ignorable value, neither true nor false"])
        ]
    , Doc.Paragraph [Doc.Text "Nothing else may be ignored."]
    ]

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
