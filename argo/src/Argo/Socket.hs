{-# Language OverloadedStrings #-}
module Argo.Socket (serveSocket) where

import Control.Concurrent       (forkFinally)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception        (displayException)
import Control.Monad            (forever)
import System.IO                (Handle, IOMode(ReadWriteMode), hClose, hPutStrLn)
import qualified Data.Map as Map

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
  Maybe Handle  {- ^ logging handle    -} ->
  N.HostName    {- ^ host              -} ->
  N.ServiceName {- ^ port              -} ->
  App s         {- ^ rpc application   -} ->
  IO ()         {- ^ start application -}
serveSocket logH hostName serviceName app =

     -- resolve listener addresses, throws exception on failure
  do infos <- N.getAddrInfo (Just hints) (Just hostName) (Just serviceName)

     -- open listener sockets on all matched addresses, typically
     -- one per address family.
     forConcurrently_ infos $ \info ->
       do s <- startListening info
          forever (acceptClient logH app s)


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
acceptClient :: Maybe Handle -> App s -> N.Socket -> IO ()
acceptClient logH app s =

  do (c, peer) <- N.accept s
     h         <- N.socketToHandle c ReadWriteMode
     -- don't use c after this, it is owned by h

     log ("CONNECT: " ++ show peer ++ "\n")
     forkFinally (serveHandlesNS logH h h app) $ \res ->
       do case res of
            Right _ -> log ("CLOSE: " ++ show peer)
            Left e  -> log ("ERROR: " ++ show peer ++ " " ++ displayException e)
          hClose h

     return ()

  where
    log =
      case logH of
        Nothing -> const (return ())
        Just h -> hPutStrLn h


-- | Hints used by 'serveSocket' specifying a stream socket intended for
-- listening for new connections.
hints :: N.AddrInfo
hints =
  N.defaultHints
    { N.addrFlags      = [N.AI_PASSIVE, N.AI_ADDRCONFIG]
    , N.addrSocketType = N.Stream -- TCP
    }
