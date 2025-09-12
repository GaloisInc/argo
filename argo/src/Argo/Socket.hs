{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
module Argo.Socket
  ( serveSocket
  , serveSocketDynamic
  ) where

import Control.Concurrent       (forkFinally)
import Control.Concurrent.Async (Async, async, forConcurrently_)
import Control.Exception        (displayException)
import Control.Monad            (forever)
import qualified Data.Text as T
import System.IO                (IOMode(ReadWriteMode), hClose)

import qualified Network.Socket as N

import Argo (App, MethodOptions(..), serveHandlesNS)

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
  MethodOptions     {- ^ options for how to execute methods -} ->
  N.HostName        {- ^ host              -} ->
  N.ServiceName     {- ^ port              -} ->
  App s             {- ^ rpc application   -} ->
  IO ()             {- ^ start application -}
serveSocket opts hostName serviceName app =

     -- resolve listener addresses, throws exception on failure
  do (infos :: [N.AddrInfo]) <-
       N.getAddrInfo (Just hints) (Just hostName) (Just serviceName)

     -- open listener sockets on all matched addresses, typically
     -- one per address family.
     forConcurrently_ infos $ \info ->
       do s <- startListening info
          forever (acceptClient opts app s)

-- | Start listening on a single, dynamically assigned port.
-- The resulting worker thread and dynamically assigned port
-- number are returned on success.
serveSocketDynamic ::
  MethodOptions     {- ^ options for how to execute methods -} ->
  N.HostName        {- ^ IP address        -} ->
  App s             {- ^ RPC application   -} ->
  IO (Async (), N.PortNumber)
serveSocketDynamic opts hostName app =

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
     a <- async (forever (acceptClient opts app s))
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
acceptClient :: MethodOptions -> App s -> N.Socket -> IO ()
acceptClient opts app s =

  do (c, peer) <- N.accept s
     h         <- N.socketToHandle c ReadWriteMode
     let logMessage = optLogger opts
     -- don't use c after this, it is owned by h

     logMessage (T.pack ("CONNECT: " ++ show peer))
     _ <- forkFinally (serveHandlesNS opts h h app) $ \res ->
       do case res of
            Right _ -> logMessage (T.pack ("CLOSE: " ++ show peer))
            Left e  -> logMessage (T.pack ("ERROR: " ++ show peer ++ " " ++ displayException e))
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
