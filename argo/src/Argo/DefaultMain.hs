{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Argo.DefaultMain (defaultMain) where


import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Foldable
import Control.Concurrent.Async (wait)
import Control.Exception (handle, IOException)
import Network.Socket (HostName)
import qualified Options.Applicative as Opt
import Safe (readMay)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.FileLock
import System.Directory
import System.FilePath
import System.Exit (die)

import Argo
import Argo.Socket


defaultMain :: String -> App s -> IO ()
defaultMain str app =
  do opts <- Opt.execParser (options str)
     realMain app opts

data Options =
  Options
    { transportOpt :: TransportOpt
    }

newtype Port = Port String deriving (Eq, Ord)

data TransportOpt
  = StdIONetstring
  -- ^ NetStrings over standard IO
  | SocketNetstring (Maybe Session) (Maybe Publicity) (Maybe Port)
  -- ^ NetStrings over some socket

data Publicity
  = Public

newtype Session
  = Session String

options :: String -> Opt.ParserInfo Options
options desc =
  Opt.info ((Options <$> transport) <**> Opt.helper) $
  Opt.fullDesc <> Opt.progDesc desc

transport :: Opt.Parser TransportOpt
transport =
  (\p pub s -> SocketNetstring s pub p)
    <$> port <*> publicity <*> session
  <|> stdio
  where
    port = (Just . Port <$>
      Opt.strOption
        (Opt.long "port" <>
         Opt.metavar "PORT" <>
         Opt.help "Use netstrings over a socket on PORT to communicate"))
      <|> pure Nothing

    stdio  = Opt.flag' StdIONetstring $
             Opt.long "stdio" <> Opt.help "Use netstrings over stdio to communicate"

    publicity =
      Opt.flag Nothing (Just Public) $
      Opt.long "public" <> Opt.help "Make the server available over the network"

    session =
      (fmap (Just . Session) . Opt.strOption $
       Opt.long "session" <>
       Opt.metavar "NAME" <>
       Opt.help "Create or look up a globally available session")
      <|> pure Nothing

selectHost :: Maybe Publicity -> HostName
selectHost (Just Public) = "::"
selectHost Nothing       = "::1"

data SessionResult
  = MakeNew Port
  | MakeNewDyn (Port -> IO ())
  | UseExisting Port
  | MismatchesExisting Port Port

getOrLockSession :: Maybe Session -> Maybe Publicity -> Maybe Port -> IO SessionResult
getOrLockSession Nothing _ Nothing     = pure (MakeNewDyn (\_ -> pure ()))
getOrLockSession Nothing _ (Just port) = pure (MakeNew port)
getOrLockSession (Just (Session session)) publicity maybePort =
  do argoDataDir <- getAppUserDataDirectory "argo"
     let sessionsDir = argoDataDir </> "sessions"
     createDirectoryIfMissing True sessionsDir
     let prefix =
           maybe id (const (<.> "public")) publicity $
           sessionsDir </> session
         globalLockFile  = argoDataDir </> "global.lock"
         sessionLockFile = prefix <.> "lock"
         portFile        = prefix <.> "port"

     -- Only one process at a time can execute the below section:
     globalLock <- lockFile globalLockFile Exclusive

     -- Clean up previous inactive sessions
     sessionLockFiles <-
       map (sessionsDir </>) .
       filter ("lock" `isExtensionOf`) <$>
       listDirectory sessionsDir
     for_ sessionLockFiles $ \lockFilePath ->
       handle (\(_ :: IOException) -> pure ()) $
         do unlocked <- fmap isJust <$>
              withTryFileLock lockFilePath Exclusive $ \_ ->
                removeFile (lockFilePath -<.> "port")
            when unlocked (removeFile lockFilePath)

     -- Attempt to lock the session, non-blockingly
     sessionLockResult <- tryLockFile sessionLockFile Exclusive
     case sessionLockResult of

       -- If another process is already locked on this session:
       Nothing ->
         -- With a read lock on the port file for this session...
         handle
           -- Read the port file and parse it
           (\(_ :: IOException) -> die $ "Could not read port file " <> portFile)
           (do parsedPort <- fmap Port . readMay <$> readFile portFile
               unlockFile globalLock
               existingPort <-
                 maybe (die $ "Could not parse port file " <> portFile)
                       pure
                       parsedPort
               -- Make sure it matches the pre-existing session's port
               pure $ maybe
                 (UseExisting existingPort)
                 (\desiredPort ->
                    if desiredPort == existingPort
                    then UseExisting existingPort
                    else MismatchesExisting existingPort desiredPort)
                 maybePort)

       -- If we are the first process to try to create/get this session:
       Just _runningLock ->  -- lock implicitly held till end of process
         case maybePort of

           -- If the user specified a port:
           Just (Port port) ->
             -- Write the specified port to the port file
             do writeFile portFile (show port)
                unlockFile globalLock
                pure (MakeNew (Port port))

           -- If the user didn't specify a port:
           Nothing ->
             do -- Return a function that takes in a port, writes the port to
                -- the (already locked) port file, and unlocks the main lock so
                -- another critical section can run
                pure . MakeNewDyn $ \(Port port) ->
                  do writeFile portFile (show port)
                     unlockFile globalLock

realMain :: App s -> Options -> IO ()
realMain theApp opts =
  case transportOpt opts of
    StdIONetstring ->
      serveStdIONS theApp
    SocketNetstring sessionOpt publicityOpt portOpt ->
      do sessionResult <- getOrLockSession sessionOpt publicityOpt portOpt
         hSetBuffering stdout NoBuffering
         case sessionResult of
           UseExisting (Port port) ->
             putStrLn ("PORT " ++ port)
           MakeNew (Port port) ->
             do putStrLn ("PORT " ++ port)
                serveSocket (selectHost publicityOpt) port theApp
           MakeNewDyn registerPort ->
             do let h = selectHost publicityOpt
                (a, port) <- serveSocketDynamic h theApp
                registerPort (Port (show port))
                putStrLn ("PORT " ++ show port)
                wait a
           MismatchesExisting (Port existingPort) (Port desiredPort) ->
             die $ "Named session already exists, but is running on port "
                   <> existingPort
                   <> ", not the specified port "
                   <> desiredPort
