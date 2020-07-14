{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | The server state manages the various application states that are
-- available to clients.
module Argo.ServerState (
  -- * The server's state, which wraps app states
  ServerState,
  initialState,
  saveNewState,
  -- ** Lenses into the server's state
  appStateCache,
  -- * File caching
  freshStateReader,
  -- * Lookup operations
  recipeFile, getAppState,
  -- * Identifiers for states
  StateID, initialStateID
  ) where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON
import Data.IORef
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Argo.Panic

import GHC.IO.Exception

import qualified Crypto.Hash.SHA1 as SHA1 (hash)

-- | A representation of protocol states to be exchanged with clients
data StateID = InitialStateID | StateID UUID
  deriving (Eq, Ord, Show)

instance Hashable StateID where
  hashWithSalt salt InitialStateID =
    salt `hashWithSalt`
    (0 :: Int) `hashWithSalt`
    ()
  hashWithSalt salt (StateID uuid) =
    salt `hashWithSalt`
    (1 :: Int) `hashWithSalt`
    uuid

-- | The state ID at the beginning of the interaction, represented as "null" in JSON
initialStateID :: StateID
initialStateID = InitialStateID

instance JSON.ToJSON StateID where
  toJSON (StateID uuid) = JSON.String (UUID.toText uuid)
  toJSON InitialStateID = JSON.Null

instance JSON.FromJSON StateID where
  parseJSON JSON.Null = pure InitialStateID
  -- Note: this is a backwards compatibility hack that should be removed soon
  parseJSON (JSON.Array arr) | arr == mempty = pure InitialStateID
  parseJSON other = subsequentState other
    where
      subsequentState =
        JSON.withText "state" $
         \txt ->
           case UUID.fromText txt of
             Nothing -> mempty
             Just uuid -> pure (StateID uuid)

newtype FileHash = FileHash ByteString deriving (Eq, Ord, Show, Hashable)

-- | Given a recipe step and its resulting application state, save
-- them to the cache and return a fresh state ID that describes the
-- resulting state.
saveNewState ::
  HasCallStack =>
  ServerState recipeStep s ->
  StateID {-^ The parent state -} ->
  recipeStep {-^ The step taken to achieve the new state -} ->
  s {-^ The new application state -} ->
  IO StateID
saveNewState server parent step appState =
  do uuid <- UUID.nextRandom
     validateStateID parent
     modifyIORef' (view stateRecipes server) $ set (at uuid) (Just (step, parent))
     let sid = StateID uuid
     modifyIORef' (view appStateCache server) $ set (at sid) (Just appState)
     return sid
  where
    validateStateID :: HasCallStack => StateID -> IO ()
    validateStateID InitialStateID = pure ()
    validateStateID (StateID uuid) =
      do recipes <- readIORef (view stateRecipes server)
         case view (at uuid) recipes of
           Nothing -> panic "saveNewState" ["Parent state ID not found"]
           Just _ -> pure ()

-- | Read a file and its hash simultaneously
readWithHash :: FilePath -> IO (ByteString, FileHash)
readWithHash file =
  do contents <- B.readFile file
     let fileHash = FileHash $! SHA1.hash contents
     return (contents, fileHash)

-- | A file reading operation that should be used when a state is
-- fresh; that is, when a state is being constructed for the first
-- time rather than when the client has sent its ID. This is intended
-- to be applied to its first two arguments by the server, and
-- supplied to an app as a @FilePath -> IO ByteString@.
freshStateReader ::
  ServerState recipeStep s -> StateID -> FilePath -> IO ByteString
freshStateReader server sid fileName =
  do (contents, fileHash) <- readWithHash fileName
     modifyIORef' (view stateFiles server) $ HM.insert (sid, fileName) fileHash
     modifyIORef' (view stateFileContents server) $ HM.insert fileHash contents
     return contents

-- | Server states contain cached application states and manage state
-- identifiers. They are intended to be guarded by an 'MVar' to
-- prevent the 'IORef's from being mutated inconsistently.
data ServerState recipeStep s =
  ServerState
  { _initialAppState :: !s
    -- ^ The application state that corresponds to the empty recipe
  , _stateRecipes :: !(IORef (HashMap UUID (recipeStep, StateID)))
    -- ^ A mapping from non-initial state IDs to recipes that can be
    -- used to reconstruct them. Here, a recipe consists of the
    -- parent's state ID and the step that was taken to arrive at the
    -- present state.
  , _stateFiles :: !(IORef (HashMap (StateID, FilePath) FileHash))
    -- ^ A mapping from complete recipes to the relevant state of the
    -- filesystem at the time of their initial execution. We expect
    -- files to be much smaller than application states in practice,
    -- so we can reconstruct application states on demand.
  , _stateFileContents :: !(IORef (HashMap FileHash ByteString))
    -- ^ De-duplicated file contents (see '_stateFiles').
  , _appStateCache :: !(IORef (HashMap StateID s))
    -- ^ Some saved application states - this is essentially a partial
    -- memo table for the interpreter that can reconstruct states.
  }

-- | Construct an initial server state, given a means of constructing
-- the initial application state.
--
-- The initial application state is associated with the empty recipe
-- @[]@ and the initial 'StateID'.
initialState ::
  ((FilePath -> IO ByteString) -> IO s) ->
  IO (ServerState recipeStep s)
initialState mkS =
  do recs <- newIORef $ HM.empty
     files <- newIORef HM.empty
     fileContents <- newIORef HM.empty
     s <- mkS $ initFileReader files fileContents
     initCache <- newIORef $ HM.fromList [(initialStateID, s)]
     pure $
       ServerState
       {  _initialAppState = s
       , _stateRecipes = recs
       , _stateFiles = files
       , _stateFileContents = fileContents
       , _appStateCache = initCache
       }
  where
    initFileReader files fileContents path =
      do (contents, fileHash) <- readWithHash path
         modifyIORef' files $ HM.insert (initialStateID, path) fileHash
         modifyIORef' fileContents $ HM.insert fileHash contents
         return contents

initialAppState :: Lens' (ServerState recipeStep s) s
initialAppState = lens _initialAppState (\s a -> s { _initialAppState = a })

stateRecipes :: Lens' (ServerState recipeStep s) (IORef (HashMap UUID (recipeStep, StateID)))
stateRecipes = lens _stateRecipes (\s r -> s { _stateRecipes = r })

-- | A lens into the server's cache of application states. It is save
-- to remove entries from this cache.
appStateCache :: Lens' (ServerState recipeStep s) (IORef (HashMap StateID s))
appStateCache = lens _appStateCache (\s c -> s { _appStateCache = c })

stateFiles :: Lens' (ServerState recipeStep s) (IORef (HashMap (StateID, FilePath) FileHash))
stateFiles = lens _stateFiles (\s fs -> s { _stateFiles = fs })

stateFileContents :: Lens' (ServerState recipeStep s) (IORef (HashMap FileHash ByteString))
stateFileContents = lens _stateFileContents (\s fc -> s { _stateFileContents = fc })

-- | Retrieve the contents of a file as they were in a particular state.
recipeFile ::
  ServerState recipeStep s ->
  StateID ->
  FilePath -> IO ByteString
recipeFile server sid filename =
  do hashes <- readIORef $ view stateFiles server
     theHash <-
       case view (at (sid, filename)) hashes of
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


-- | Retrieve the application state that corresponds to a given state ID.
--
-- If the state ID is not known, returns Nothing.
getAppState ::
  (HasCallStack, Eq recipeStep, Hashable recipeStep) =>
  (s -> recipeStep -> (FilePath -> IO ByteString) -> IO s) {- ^ How to take one step of the recipe -} ->
  ServerState recipeStep s ->
  StateID ->
  IO (Maybe s)
getAppState _ server InitialStateID = pure $ Just $ view initialAppState server
getAppState runStep server sid@(StateID uuid) =
     -- First check whether the state itself is cached. If so, return it.
  do caches <- readIORef $ view appStateCache server
     case view (at sid) caches of
       Just s -> return $ Just s
       Nothing ->
         -- If it is not cached, then reconstruct it by playing back history.
         do recipes <- readIORef $ view stateRecipes server
            case view (at uuid) recipes of
              Nothing -> return Nothing
              Just (step, parent) ->
                do s <- getAppState runStep server parent
                   traverse (\s' -> runStep s' step (recipeFile server parent)) s
