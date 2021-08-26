{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | The server state manages the various application states that are
-- available to clients.
module Argo.ServerState (
  -- * The server's state, which wraps app states
  ServerState,
  initServerState,
  nextAppState,
  getAppState,
  destroyAppState,
  destroyLeastRecentState,
  destroyAllAppStates,
  statePoolCount,
  -- * Server launch options
  StateMutability(..),
  -- * Identifiers for states
  StateID, initialStateID,
  ) where

import Control.Concurrent
import Control.Monad (when)
import Numeric.Natural ( Natural )
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

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




-- | Describes whether the application state is pure or mutable.
data StateMutability
  = PureState
  | MutableState

-- | How the initial app state is stored, based on the state's mutability.
data InitialAppState appState
  = PureInitState appState
  | MutableInitState (() -> IO appState)

-- | Index for ordering states, e.g., to know which is oldest.
newtype Index = Index {indexNat :: Natural}
  deriving (Eq, Show, Ord)

-- | @ServerState@s contain cached application states and manage state
-- identifiers. They are intended to be guarded by an 'MVar' to
-- prevent the 'IORef's from being mutated inconsistently.
data ServerState appState =
  ServerState
  { serverInitAppState :: !(InitialAppState appState)
  -- ^ State entered when server is started.
  , serverStatePool :: !(IORef (HashMap UUID (Index, appState)))
  -- ^ Currently active states and their order int.
  , serverStateOrdering :: !(IORef (Set (Index, UUID)))
  -- ^ Set used to provide constant-time oldest state calculation
  -- (i.e., since pair ordering is calculated pointwise left-to-right).
  , serverIndex :: !(IORef Index)
  }

-- | Get the next index and increment the server's index.
nextIndex :: ServerState appState -> IO Index
nextIndex server = do
  idx <- readIORef $ serverIndex server
  writeIORef (serverIndex server) (Index $ (indexNat idx) + 1)
  pure idx

-- | Construct an initial server state, given a means of constructing
-- the initial application state and a bound on the number of states 
-- to keep cached.
initServerState ::
  StateMutability ->
  ((FilePath -> IO ByteString) -> IO appState) ->
  IO (ServerState appState)
initServerState mut mkInitState = do
  initState <- case mut of
    PureState -> PureInitState <$> (mkInitState B.readFile)
    MutableState -> pure $ MutableInitState $ \_ -> mkInitState $ B.readFile
  emptyStatePool <- newIORef HM.empty
  emptyStateOrdering <- newIORef Set.empty
  idx <- newIORef (Index 0)
  pure $
    ServerState
    { serverInitAppState = initState
    , serverStatePool = emptyStatePool
    , serverStateOrdering = emptyStateOrdering
    , serverIndex = idx
    }

statePoolCount :: ServerState appState -> IO Natural
statePoolCount server =
  fromInteger . toInteger . HM.size <$> readIORef (serverStatePool server)

-- | Given a fresh application state and the StateID it was derived from, return
-- a fresh state ID that uniquely describes the new state and destroy the
-- previous state (if possible). This ASSUMES the server state MVar is
-- already locked elsewhere thus preventing inconsistent server states.
nextAppState ::
  ServerState appState ->
  StateID {-^ State ID from which the new state originated (i.e., its parent) -} ->
  appState {-^ The new application state -} ->
  IO StateID
nextAppState server prevStateID newAppState = do
  destroyAppState' server prevStateID
  uuid <- UUID.nextRandom
  idx <- nextIndex server
  modifyIORef' (serverStatePool server) $ HM.insert uuid (idx, newAppState)
  modifyIORef' (serverStateOrdering server) $ Set.insert (idx, uuid)
  return $ StateID uuid

-- | Like @destroyAppState@ but ASSUMES the concerns regarding server state
-- consistency have already been handled (i.e., someone has already acquired the
-- MVar for the server state).
destroyAppState' ::
  ServerState s ->
  StateID ->
  IO ()
destroyAppState' _server InitialStateID = pure ()
destroyAppState' server (StateID uuid) = do
  HM.lookup uuid <$> readIORef (serverStatePool server) >>= \case
    Nothing -> pure ()
    Just (idx, _) -> modifyIORef' (serverStateOrdering server) $ Set.delete (idx, uuid)
  modifyIORef' (serverStatePool server) $ HM.delete uuid

-- | Destroy a non-initial app state so it is no longer available for requests.
-- Explicitly requires the @MVar (ServerState s)@ so a notification---an action
-- which does not inherently require a lock on the server's state per se---can
-- delete a state while being sure some process isn't actively operating on it
-- as well.
destroyAppState ::
  MVar (ServerState s) ->
  StateID ->
  IO ()
destroyAppState serverMVar sid =
  withMVar serverMVar $ \server -> destroyAppState' server sid


-- | Destroys the oldest state currently in cache. N.B., this function
-- assumes the caller is maintaining invariants related to MVars
-- and state shared between threads.
destroyLeastRecentState ::
  ServerState s ->
  IO ()
destroyLeastRecentState server = do
  orderSet <- readIORef (serverStateOrdering server)
  when (Set.size orderSet > 0) $ do
    let (_, uuid) = Set.elemAt 0 orderSet
    destroyAppState' server (StateID uuid)

-- | Like @destroyAppState@ but destroys all non-initial app states.
destroyAllAppStates ::
  MVar (ServerState s) ->
  IO ()
destroyAllAppStates serverMVar =
  withMVar serverMVar $ \server ->
    writeIORef (serverStatePool server) $ HM.empty

-- | Retrieve the application state that corresponds to a given state ID.
--
--   If given the initial state ID and the server state is mutable,
--   a new initial state will be returned.
--
-- If the state ID is not known, returns Nothing.
getAppState ::
  ServerState appState ->
  StateID ->
  IO (Maybe appState)
getAppState server InitialStateID =
  case (serverInitAppState server) of
    PureInitState s -> pure $ Just s
    MutableInitState f -> Just <$> f ()
getAppState server (StateID uuid) = do
  pool <- readIORef (serverStatePool server)
  pure $ snd <$> (HM.lookup uuid pool)
