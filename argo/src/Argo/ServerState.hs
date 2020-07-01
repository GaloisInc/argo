{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Argo.ServerState (ServerState, StateID, getAppState, initialState, freshStateReader, appStateCache, stateRecipe, stateRecipes, initialStateID, newStateID, recipeFile) where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON
import Data.Text (Text)
import Data.IORef
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import GHC.IO.Exception

import qualified Crypto.Hash.SHA1 as SHA1 (hash)

newtype StateID = StateID (Maybe UUID) deriving (Eq, Ord, Show, Hashable)

initialStateID :: StateID
initialStateID = StateID Nothing

newStateID :: IO StateID
newStateID = StateID . Just <$> UUID.nextRandom

instance JSON.ToJSON StateID where
  toJSON (StateID (Just uuid)) = JSON.String (UUID.toText uuid)
  toJSON (StateID Nothing) = JSON.Null

instance JSON.FromJSON StateID where
  parseJSON JSON.Null = pure $ StateID Nothing
  -- Note: this is a backwards compatibility hack that should be removed soon
  parseJSON (JSON.Array arr) | arr == mempty = pure $ StateID Nothing
  parseJSON other = subsequentState other
    where
      subsequentState =
        JSON.withText "state" $
         \txt ->
           case UUID.fromText txt of
             Nothing -> mempty
             Just uuid -> pure (StateID (Just uuid))

newtype FileHash = FileHash ByteString deriving (Eq, Ord, Show, Hashable)

readWithHash :: FilePath -> IO (ByteString, FileHash)
readWithHash file =
  do contents <- B.readFile file
     let hash = FileHash $! SHA1.hash contents
     return (contents, hash)

freshStateReader ::
  (Hashable recipeStep, Eq recipeStep) =>
  ServerState recipeStep s -> [recipeStep] -> FilePath -> IO ByteString
freshStateReader server steps fileName =
  do (contents, hash) <- readWithHash fileName
     modifyIORef' (view stateFiles server) $ HM.insert (steps, fileName) hash
     modifyIORef' (view stateFileContents server) $ HM.insert hash contents
     return contents

data ServerState recipeStep s =
  ServerState
  { _nextStateID :: !StateID
  , _initialAppState :: !s
  , _stateRecipes :: !(IORef (HashMap StateID [recipeStep]))
  , _stateFiles :: !(IORef (HashMap ([recipeStep], FilePath) FileHash))
  , _stateFileContents :: !(IORef (HashMap FileHash ByteString))
  , _appStateCache :: !(IORef (HashMap [recipeStep] s))
  }


initialState ::
  (Hashable recipeStep, Eq recipeStep) =>
  ((FilePath -> IO ByteString) -> IO s) ->
  IO (ServerState recipeStep s)
initialState mkS =
  do let theID = initialStateID
     let initRecipe = []
     recs <- newIORef $ HM.fromList [(theID, initRecipe)]
     files <- newIORef HM.empty
     fileContents <- newIORef HM.empty
     s <- mkS $ initFileReader files fileContents
     initCache <- newIORef $ HM.fromList [(initRecipe, s)]
     pure $
       ServerState
       { _nextStateID = theID
       , _initialAppState = s
       , _stateRecipes = recs
       , _stateFiles = files
       , _stateFileContents = fileContents
       , _appStateCache = initCache
       }
  where
    initFileReader files fileContents path =
      do (contents, hash) <- readWithHash path
         modifyIORef' files $ HM.insert ([], path) hash
         modifyIORef' fileContents $ HM.insert hash contents
         return contents

stateRecipes :: Lens' (ServerState recipeStep s) (IORef (HashMap StateID [recipeStep]))
stateRecipes = lens _stateRecipes (\s r -> s { _stateRecipes = r })

appStateCache :: Lens' (ServerState recipeStep s) (IORef (HashMap [recipeStep] s))
appStateCache = lens _appStateCache (\s c -> s { _appStateCache = c })

initialAppState :: Lens' (ServerState recipeStep s) s
initialAppState = lens _initialAppState (\s a -> s { _initialAppState = a })

stateFiles :: Lens' (ServerState recipeStep s) (IORef (HashMap ([recipeStep], FilePath) FileHash))
stateFiles = lens _stateFiles (\s fs -> s { _stateFiles = fs })

stateFileContents :: Lens' (ServerState recipeStep s) (IORef (HashMap FileHash ByteString))
stateFileContents = lens _stateFileContents (\s fc -> s { _stateFileContents = fc })

stateRecipe :: ServerState recipeStep s -> StateID -> IO (Maybe [recipeStep])
stateRecipe server sid =
  do recipes <- readIORef $ view stateRecipes server
     pure $ HM.lookup sid recipes


recipeFile ::
  (Eq recipeStep, Hashable recipeStep) =>
  ServerState recipeStep s ->
  [recipeStep] ->
  FilePath -> IO ByteString
recipeFile server recipe filename =
  do hashes <- readIORef $ view stateFiles server
     theHash <-
       case view (at (recipe, filename)) hashes of
         Nothing ->
           ioError $
           IOError { ioe_handle = Nothing
                   , ioe_type = NoSuchThing
                   , ioe_location = "restoring " ++ filename
                   , ioe_description = "No such cached file " ++ filename
                   , ioe_errno = Nothing
                   , ioe_filename = Just filename
                   }
         Just h -> pure h
     fileContents <- readIORef $ view stateFileContents server
     case view (at theHash) fileContents of
       Nothing ->
         ioError $
         IOError { ioe_handle = Nothing
                 , ioe_type = NoSuchThing
                 , ioe_location = "restoring " ++ filename
                 , ioe_description = "No contents for cached file " ++ filename
                 , ioe_errno = Nothing
                 , ioe_filename = Just filename
                 }

       Just bs -> pure bs


getAppState ::
  (Eq recipeStep, Hashable recipeStep) =>
  (s -> recipeStep -> (FilePath -> IO ByteString) -> IO s) ->
  ServerState recipeStep s ->
  [recipeStep] ->
  IO s
getAppState runStep server recipe =
  do recipeCache <- readIORef $ view appStateCache server
     case view (at recipe) recipeCache of
       Just s -> return s
       Nothing -> runRecipe runStep server recipe

runRecipe ::
  (Eq recipeStep, Hashable recipeStep) =>
  (s -> recipeStep -> (FilePath -> IO ByteString) -> IO s) ->
  ServerState recipeStep s ->
  [recipeStep] ->
  IO s
runRecipe _ server [] = pure $ view initialAppState server
runRecipe runStep server recipe@(step : steps) =
  do s <- getAppState runStep server steps
     let fileCache = recipeFile server recipe
     runStep s step fileCache
