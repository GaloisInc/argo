{-# Language OverloadedStrings #-}
module Argo.Socket
  ( serveSocket
  , serveSocketDynamic
  ) where

import Control.Concurrent       (forkFinally)
import Control.Concurrent.Async (Async, async, forConcurrently_)
import Control.Exception        (displayException)
import Control.Monad            (forever)
import System.IO                (IOMode(ReadWriteMode), hClose, stderr)

import qualified Network.Socket as N

import Argo (App, serveHandlesNS)

-- | Arbitrarily chosen value for number of connects to hold
-- in queue during a burst of connections before they are
-- accepted by the server.
listenQueueDepth :: Int
listenQueueDepth = 10

-- | Start listening on the given address and serve the given RPC
-- application for all connecting clients. Messages are expected
-- to use netstring-wrapped JSON RPC format.
--
-- A reasonable default host name is "::", and a reasonable default
-- service name is a port number as a string, e.g. "10000".
serveSocket ::
  (String -> IO ()) {- ^ logger            -} ->
  N.HostName        {- ^ host              -} ->
  N.ServiceName     {- ^ port              -} ->
  App s             {- ^ rpc application   -} ->
  IO ()             {- ^ start application -}
serveSocket logger hostName serviceName app =

     -- resolve listener addresses, throws exception on failure
  do infos <- N.getAddrInfo (Just hints) (Just hostName) (Just serviceName)

     -- open listener sockets on all matched addresses, typically
     -- one per address family.
     forConcurrently_ infos $ \info ->
       do s <- startListening info
          forever (acceptClient logger app s)

-- | Start listening on a single, dynamically assigned port.
-- The resulting worker thread and dynamically assigned port
-- number are returned on success.
serveSocketDynamic ::
  (String -> IO ()) {- ^ Logger            -} ->
  N.HostName        {- ^ IP address        -} ->
  App s             {- ^ RPC application   -} ->
  IO (Async (), N.PortNumber)
serveSocketDynamic logger hostName app =

     -- resolve listener addresses, throws exception on failure
  do let hint1 =
           N.defaultHints
             { N.addrFlags      = [N.AI_NUMERICHOST, N.AI_ADDRCONFIG]
             , N.addrSocketType = N.Stream }
     infos <- N.getAddrInfo (Just hint1) (Just hostName) Nothing
     info  <- case infos of
       [info] -> return info
       _      -> fail "serveSocketDynamic: host resolved as too many addresses"

     s <- startListening info
     a <- async (forever (acceptClient logger app s))
     p <- N.socketPort s
     return (a, p)


-- | Create a new listening socket for this address.
startListening :: N.AddrInfo -> IO N.Socket
startListening addr =
  do s <- N.socket (N.addrFamily     addr)
                   (N.addrSocketType addr)
                   (N.addrProtocol   addr)
     N.setSocketOption s N.ReuseAddr 1 -- easier to restart server
     N.bind s (N.addrAddress addr)
     N.listen s listenQueueDepth
     return s


-- | Accept a new connection on the given listening socket and
-- start processing rpc requests.
acceptClient :: (String -> IO ()) -> App s -> N.Socket -> IO ()
acceptClient logMessage app s =

  do (c, peer) <- N.accept s
     h         <- N.socketToHandle c ReadWriteMode
     -- don't use c after this, it is owned by h

     logMessage ("CONNECT: " ++ show peer)
     _ <- forkFinally (serveHandlesNS (Just stderr) h h app) $ \res ->
       do case res of
            Right _ -> logMessage ("CLOSE: " ++ show peer)
            Left e  -> logMessage ("ERROR: " ++ show peer ++ " " ++ displayException e)
          hClose h

     return ()


-- | Hints used by 'serveSocket' specifying a stream socket intended for
-- listening for new connections.
hints :: N.AddrInfo
hints =
  N.defaultHints
    { N.addrFlags      = [N.AI_PASSIVE, N.AI_ADDRCONFIG]
    , N.addrSocketType = N.Stream -- TCP
    }
