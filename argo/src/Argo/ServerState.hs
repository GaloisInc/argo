{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | The server state manages the various application states that are
-- available to clients.
module Argo.ServerState (
  -- * The server's state, which wraps app states
  ServerState,
  initServerState,
  saveNewAppState,
  -- * Server launch options
  ServerOpts, defaultServerOpts, optEphemeralCacheLimit,
  -- * File reader with caching
  freshStateFileReader,
  -- * Lookup operations
  getFile, getAppState,
  -- * Identifiers for states
  StateID, initialStateID,
  -- * Server state/cache management
  pinAppState, unpinAppState,
  clearPersistentCache, setEphemeralCacheLimit,
  ephemeralCacheMembers, persistentCacheMembers
  ) where

import Control.Lens
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, getCurrentTime)
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

-- | Given a fresh application state, return a fresh state
-- ID that uniquely describes it.
saveNewAppState ::
  ServerState appState ->
  appState {-^ The new application state -} ->
  IO StateID
saveNewAppState server newAppState =
  do uuid <- UUID.nextRandom
     now <- getCurrentTime
     let cas = CachedAppState
               { casState = newAppState
               , casFiles = HM.empty
               , casLastUsed = now
               }
     enforceCacheLimit server
     modifyIORef' (view ephemeralStateCache server) $ HM.insert uuid cas
     return $ StateID uuid


-- If we are at or over capacity, remove the oldest item(s)
-- from the ephemeral cache until we are at half capacity.
enforceCacheLimit :: ServerState s -> IO ()
enforceCacheLimit server = do
  maxSize <- readIORef (view ephemeralStateCacheLimit server)
  curSize <- HM.size <$> readIORef (view ephemeralStateCache server)
  if (curSize >= maxSize) then do
    entries <- HM.toList <$> readIORef (view ephemeralStateCache server)
    -- Remove at least one, remove enough to get under max size, and
    -- remove some extra to keep us under the cap for a bit if possible.
    let n = max 1 $ max (curSize - maxSize) (maxSize `div` 2)
    let extras = take n $ sortOn (casLastUsed . snd) entries
    forM_ extras $ \(uuid, cas) -> do
      -- Remove the state from the ephemeral cache.
      modifyIORef' (view ephemeralStateCache server) $ HM.delete uuid
      -- Decrement the reference tracking sets for the cached files.
      mapM_ (removeRefToFile uuid) $ casFiles cas
  else pure ()

 where
    -- Update the cached file corresponding to @fHash@ so it no longer thinks it
    -- is referred to by @uuid@.
    removeRefToFile :: UUID -> FileHash -> IO ()
    removeRefToFile uuid fHash = do
      (HM.lookup fHash <$> (readIORef (view cachedFiles server))) >>=
        \case
          Nothing -> pure ()
          Just cf -> do
            let states = Set.delete (StateID uuid) (cfStates cf)
            if Set.null states
            then modifyIORef' (view cachedFiles server) $ HM.delete fHash
            else modifyIORef' (view cachedFiles server) $ HM.insert fHash (cf {cfStates = states})



-- | Read a file and its hash simultaneously
readWithHash :: FilePath -> IO (ByteString, FileHash)
readWithHash file =
  do contents <- B.readFile file
     let fileHash = FileHash $! SHA1.hash contents
     return (contents, fileHash)

-- | A file reading operation that should be used to read a file
-- for the first time for a state (i.e., when the state is
-- fresh and is being constructed for the first time rather than
-- when the client has sent its ID). This is intended to be 
-- applied to its first two arguments by the server, and
-- supplied to an app as a @FilePath -> IO ByteString@.
freshStateFileReader ::
  ServerState s -> StateID -> FilePath -> IO ByteString
freshStateFileReader server sid fileName = do
  let cFilesRef = view cachedFiles server
  (contents, fileHash) <- readWithHash fileName
  -- Update the cached files map as appropriate.
  (HM.lookup fileHash <$> (readIORef cFilesRef)) >>=
    \case
      Just _ -> do
        -- We have _already_ seen this file before! Add this `sid` to the set
        -- of states which refer to this file.
        let updateCF = \cf -> cf {cfStates = Set.insert sid (cfStates cf)}
        modifyIORef' cFilesRef $ HM.adjust updateCF fileHash
      Nothing -> do
        -- We have _not_ seen this file before. Add it to the cached files.
        modifyIORef' cFilesRef $ 
          HM.insert fileHash (CachedFile contents (Set.singleton sid))
  -- Find and update the cached app state.
  case sid of
    InitialStateID -> do
      modifyIORef' (view cachedInitState server) $ 
        \cas -> cas {casFiles = HM.insert fileName fileHash (casFiles cas)}
    StateID uuid -> do
      (cas, casCacheRef) <- do
        (HM.lookup uuid <$> readIORef (view ephemeralStateCache server)) >>=
         \case
           Just cas -> pure (cas, (view ephemeralStateCache server))
           Nothing -> do
             (HM.lookup uuid <$> readIORef (view persistentStateCache server)) >>=
              \case
                Just cas -> pure (cas, (view persistentStateCache server))
                Nothing -> panic "readAndCacheFileForState" ["State ID not found: " ++ (show uuid)]
      let cas' = cas {casFiles = HM.insert fileName fileHash (casFiles cas)}
      modifyIORef' casCacheRef $ HM.insert uuid cas'
  pure contents


data CachedFile =
  CachedFile
  { cfContents :: !ByteString
  -- ^ File Contents
  , cfStates :: !(Set StateID)
    -- ^ States referring to this file.
  }

data CachedAppState s =
  CachedAppState
  { casState :: !s
  -- ^ The cached app state itself.  
  , casFiles :: !(HashMap FilePath FileHash)
  -- ^ Files this state relies on.
  , casLastUsed :: !UTCTime
  -- ^ When this state last involved in a server request.
  }

-- | Options affecting how the server initial settings at launch.
data ServerOpts =
  ServerOpts
  {
    initEphemeralStateCacheLimit :: !Int
  }

-- | Generic server options, including the following:
--   * Ephemeral cache size (see @optEphemeralCacheLimit@, default is @10@.)
defaultServerOpts :: ServerOpts
defaultServerOpts =
  ServerOpts
  {
    initEphemeralStateCacheLimit = 10
  }

-- | Updates the size of the ephemeral state cache for a @ServerOpts@,
-- i.e., how many non-pinned, non-initial states are kept alive by
-- the server. When the limit is reached, half of the ephemeral states
-- (the oldest) are purged from the cache and are no longer available
-- for server requests.
optEphemeralCacheLimit ::
  Int ->
  ServerOpts ->
  ServerOpts
optEphemeralCacheLimit sz opts =
  opts {
          initEphemeralStateCacheLimit = sz
       }


-- | @ServerState@s contain cached application states and manage state
-- identifiers. They are intended to be guarded by an 'MVar' to
-- prevent the 'IORef's from being mutated inconsistently.
data ServerState appState =
  ServerState
  { _cachedInitState :: !(IORef (CachedAppState appState))
  -- State entered when server is started (is retained indefinitely).
  , _cachedFiles :: !(IORef (HashMap FileHash CachedFile))
    -- ^ Cache of loaded files (allows for sharing and tracking usage).
    --   N.B., cached files should be updated and possibly purged by Argo
    --   as states that refer to them are purged (see @cachedFileStates@
    --   field of @CachedFile@).
  , _ephemeralStateCache :: !(IORef (HashMap UUID (CachedAppState appState)))
    -- ^ Live application states which are subject to being purged.
  , _persistentStateCache :: !(IORef (HashMap UUID (CachedAppState appState)))
    -- ^ Live application states which are retained until otherwise directed.
  , _ephemeralStateCacheLimit :: !(IORef Int)
  -- ^ Maximum number of application states to keep live in the ephemeral cache.
  }

-- | A lens into the cached initial server state
cachedInitState :: Lens' (ServerState appState) (IORef (CachedAppState appState))
cachedInitState = lens _cachedInitState (\s is -> s { _cachedInitState = is })

-- | A lens into the cache of files used by in-cache states.
cachedFiles :: Lens' (ServerState appState) (IORef (HashMap FileHash CachedFile))
cachedFiles = lens _cachedFiles (\s fs -> s { _cachedFiles = fs })

-- | A lens into the server's ephemoral cached application states.
ephemeralStateCache :: Lens' (ServerState appState) (IORef (HashMap UUID (CachedAppState appState)))
ephemeralStateCache = lens _ephemeralStateCache (\s c -> s { _ephemeralStateCache = c })


-- | A lens into the server's persistent cached application states.
persistentStateCache :: Lens' (ServerState appState) (IORef (HashMap UUID (CachedAppState appState)))
persistentStateCache = lens _persistentStateCache (\s c -> s { _persistentStateCache = c })

-- | A lens to the maximum number of states to keep in cache (if one exists).
ephemeralStateCacheLimit :: Lens' (ServerState appState) (IORef Int)
ephemeralStateCacheLimit = lens _ephemeralStateCacheLimit (\s sz -> s { _ephemeralStateCacheLimit = sz })


-- | Construct an initial server state, given a means of constructing
-- the initial application state and a bound on the number of states 
-- to keep cached.
--
-- The initial application state is associated with the empty recipe
-- @[]@ and the initial 'StateID'.
initServerState ::
  ServerOpts ->
  ((FilePath -> IO ByteString) -> IO appState) ->
  IO (ServerState appState)
initServerState opts mkInitState =
  do fileCache <- newIORef HM.empty
     eStateCache <- newIORef $ HM.empty
     pStateCache <- newIORef $ HM.empty
     eLimit <- newIORef (initEphemeralStateCacheLimit opts)
     initStateFiles <- newIORef $ HM.empty
     s <- mkInitState $ initFileReader fileCache initStateFiles
     sFiles <- readIORef initStateFiles
     now <- getCurrentTime
     cInitState <- newIORef $ CachedAppState s sFiles now
     pure $
       ServerState
       { _cachedInitState = cInitState
       , _cachedFiles = fileCache
       , _ephemeralStateCache = eStateCache
       , _persistentStateCache = pStateCache
       , _ephemeralStateCacheLimit = eLimit
       }
  where
    initFileReader :: IORef (HashMap FileHash CachedFile) -> 
                      IORef (HashMap FilePath FileHash) ->
                      FilePath ->
                      IO ByteString
    initFileReader fileCache initStateFiles path =
      do (contents, fileHash) <- readWithHash path
         modifyIORef' fileCache $ HM.insert fileHash (CachedFile contents (Set.singleton initialStateID))
         (HM.lookup path <$> readIORef initStateFiles) >>=
          \case
            -- We haven't seen this file before during initial state creation, so just add it
            -- and it's content to the hash maps.
            Nothing -> do
              modifyIORef' initStateFiles $ HM.insert path fileHash
            -- They read the same file twice while creating the initial state and the contents
            -- did not change... do nothing since we've already cached the contents.
            Just prevHash | prevHash == fileHash -> pure ()
            -- They've re-read this file but the contents are now different. Remove the old cached
            -- contents (since we identify files uniquely by state and filepath) and log
            -- the new one..
            Just prevHash -> do
              modifyIORef' fileCache $ HM.delete prevHash
              modifyIORef' initStateFiles $ HM.insert path fileHash
         pure contents


-- | Lookup a @CachedAppState@ (whether it's cached ephemerally or
--   persistently) and update it's last used timestamp.
getCachedAppState :: 
  ServerState s ->
  StateID ->
  IO (Maybe (CachedAppState s))
getCachedAppState server InitialStateID = do
  s <- readIORef $ view cachedInitState server
  pure $ Just s
getCachedAppState server (StateID uuid) = do
  (HM.lookup uuid <$> (readIORef $ view ephemeralStateCache server)) >>=
    \case
      Just cas -> do
        now <- getCurrentTime
        modifyIORef' (view ephemeralStateCache server) $ HM.insert uuid (cas {casLastUsed = now})
        pure $ Just cas
      Nothing -> do
        (HM.lookup uuid <$> (readIORef $ view persistentStateCache server)) >>=
          \case
            Just cas -> do
              now <- getCurrentTime
              modifyIORef' (view persistentStateCache server) $ HM.insert uuid (cas {casLastUsed = now})
              pure $ Just cas
            Nothing -> pure Nothing


-- | Retrieve the contents of a file as they were in a particular state.
getFile ::
  ServerState appState ->
  StateID ->
  FilePath -> IO ByteString
getFile server sid filename = do
  sFiles <- getCachedAppState server sid >>=
              \case
                 Nothing -> raiseNoSuchState
                 Just cState -> pure $ casFiles cState
  fHash <- case HM.lookup filename sFiles of
             Nothing -> raiseNoSuchFile
             Just fileHash -> pure fileHash
  (HM.lookup fHash <$> readIORef (view cachedFiles server)) >>=
    \case
       Nothing -> raiseNoSuchFile
       Just cf -> pure $ cfContents cf
  where
      raiseNoSuchFile =
        ioError $
         IOError { ioe_handle = Nothing
                 , ioe_type = NoSuchThing
                 , ioe_location = "restoring " ++ filename
                 , ioe_description = "No such cached file " ++ filename
                 , ioe_errno = Nothing
                 , ioe_filename = Just filename
                 }
      raiseNoSuchState =
        ioError $
         IOError { ioe_handle = Nothing
                 , ioe_type = NoSuchThing
                 , ioe_location = "restoring " ++ filename ++ " in state " ++ (show sid)
                 , ioe_description = "No such state currently cached " ++ (show sid)
                 , ioe_errno = Nothing
                 , ioe_filename = Just filename
                 }



-- | Retrieve the application state that corresponds to a given state ID.
--
-- If the state ID is not known, returns Nothing.
getAppState ::
  ServerState appState ->
  StateID ->
  IO (Maybe appState)
getAppState server sid = do
  mCAS <- getCachedAppState server sid
  pure $ casState <$> mCAS


-- | Ensure a currently visible state remains visible/in-cache
-- until explicitly unpinned.
pinAppState ::
  ServerState appState ->
  StateID ->
  IO ()
pinAppState _server InitialStateID = pure ()
pinAppState server (StateID uuid) =
  (HM.lookup uuid <$> readIORef (view ephemeralStateCache server)) >>=
    \case
      Just cas -> do
        -- Move it from the ephemeral to the persistent cache.
        now <- getCurrentTime
        modifyIORef' (view ephemeralStateCache server) $ HM.delete uuid
        modifyIORef' (view persistentStateCache server) $ HM.insert uuid (cas {casLastUsed = now})
      Nothing -> do
        -- We didn't see it in the ephemeral cache... let's see if it's already in the persistent cache.
        (HM.lookup uuid <$> readIORef (view persistentStateCache server)) >>=
          \case
            Just cas -> do
              -- Let's just update the last used time.
              now <- getCurrentTime
              modifyIORef' (view persistentStateCache server) $ HM.insert uuid (cas {casLastUsed = now})
            Nothing -> do
              -- We didn't see it in either cache... uh oh
              ioError $
                IOError { ioe_handle = Nothing
                        , ioe_type = NoSuchThing
                        , ioe_location = "pinning state " ++ (show uuid)
                        , ioe_description = "No such state currently cached " ++ (show uuid)
                        , ioe_errno = Nothing
                        , ioe_filename = Nothing
                        }


-- Like unpinAppState but takes a UUID and performs no cache flushing.
internalUnpinAppState ::
  ServerState appState ->
  UUID ->
  IO ()
internalUnpinAppState server uuid =
  (HM.lookup uuid <$> readIORef (view persistentStateCache server)) >>=
    \case
      Just cas -> do
        modifyIORef' (view ephemeralStateCache server) $ HM.insert uuid cas
        modifyIORef' (view persistentStateCache server) $ HM.delete uuid
      Nothing -> pure ()


-- | Unpin a cached state (a noop if the state is not found in the persistent cache).
unpinAppState ::
  ServerState appState ->
  StateID ->
  IO ()
unpinAppState _server InitialStateID = pure ()
unpinAppState server (StateID uuid) = do
  internalUnpinAppState server uuid
  enforceCacheLimit server

-- | Move states from the persistent cache into the ephemeral cache
-- and empty old states from the cache if necessary.
clearPersistentCache ::
  ServerState appState ->
  IO ()
clearPersistentCache server = do
  pids <- HM.keys <$> readIORef (view persistentStateCache server)
  mapM_ (internalUnpinAppState server) pids
  enforceCacheLimit server

-- | Change the ephemeral cache limit and clean the cache to meet
-- the new limit if necessary.
setEphemeralCacheLimit ::
  ServerState appState ->
  Int ->
  IO ()
setEphemeralCacheLimit server sz = do
  writeIORef (view ephemeralStateCacheLimit server) (max sz 1)
  enforceCacheLimit server

-- | Return the keys in the ephemeral cache.
ephemeralCacheMembers ::
  ServerState appState ->
  IO [UUID]
ephemeralCacheMembers server =
  HM.keys <$> readIORef (view ephemeralStateCache server)

-- | Return the keys in the persistent cache.
persistentCacheMembers ::
  ServerState appState ->
  IO [UUID]
persistentCacheMembers server =
  HM.keys <$> readIORef (view persistentStateCache server)
