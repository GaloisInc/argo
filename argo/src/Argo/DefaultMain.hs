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
  do opts <- Opt.customExecParser
               (Opt.prefs $ Opt.showHelpOnError <> Opt.showHelpOnEmpty)
               (options str)
     realMain app opts

data NetworkOptions =
  NetworkOptions (Maybe Session) (Maybe HostName) (Maybe Port)

data ProgramMode
  = StdIONetstring
  -- ^ NetStrings over standard IO
  | SocketNetstring NetworkOptions
  -- ^ NetStrings over some socket
  | Http String NetworkOptions
  -- ^ HTTP (String is path at which API is served)

newtype Port = Port {unPort :: String} deriving (Eq, Ord)

newtype Session
  = Session String

mode :: String -> Opt.Parser ProgramMode
mode desc =
  Opt.hsubparser $ stdioMode desc <> socketMode desc <> httpMode desc

stdioMode, socketMode, httpMode :: String -> Opt.Mod Opt.CommandFields ProgramMode

stdioMode desc = Opt.command "stdio" $
  Opt.info (pure StdIONetstring) $
  Opt.header desc <> Opt.fullDesc <> Opt.progDesc "Communicate over stdin and stdout"
socketMode desc = Opt.command "socket" $
  Opt.info (SocketNetstring <$> networkOptions) $
  Opt.header desc <> Opt.fullDesc <> Opt.progDesc "Communicate over a socket"
httpMode desc = Opt.command "http" $
  Opt.info (Http <$> path <*> networkOptions) $
  Opt.header desc <> Opt.fullDesc <> Opt.progDesc "Communicate over HTTP"
  where
    path =
      Opt.argument Opt.str $
      Opt.metavar "PATH" <> Opt.help "The path at which to serve the API"

networkOptions :: Opt.Parser NetworkOptions
networkOptions =
  NetworkOptions <$> sessionOpt <*> hostOpt <*> portOpt

options :: String -> Opt.ParserInfo ProgramMode
options desc =
  Opt.info (mode desc <**> Opt.helper) $
  Opt.fullDesc <> Opt.progDesc desc

-- transport :: Opt.Parser TransportOpt
-- transport =
--   (\p pub s -> SocketNetstring s pub p)
--     <$> port <*> publicity <*> session
--   <|> stdio
--   where


--     stdio  = Opt.flag' StdIONetstring $
--              Opt.long "stdio" <> Opt.help "Use netstrings over stdio to communicate"

--     publicity =
--       Opt.flag Nothing (Just Public) $
--       Opt.long "public" <> Opt.help "Make the server available over the network"

hostOpt :: Opt.Parser (Maybe HostName)
hostOpt =
  (fmap Just . Opt.strOption $
   Opt.long "host" <>
   Opt.metavar "HOSTNAME" <>
   Opt.help "Select the hostname on which to listen (use 0.0.0.0 or :: for publicly available services).")
  <|> pure Nothing

sessionOpt :: Opt.Parser (Maybe Session)
sessionOpt =
  (fmap (Just . Session) . Opt.strOption $
   Opt.long "session" <>
   Opt.metavar "NAME" <>
   Opt.help "Create or look up a globally available session")
  <|> pure Nothing


portOpt :: Opt.Parser (Maybe Port)
portOpt =
 (Just . Port <$>
  Opt.strOption
    (Opt.long "port" <>
     Opt.metavar "PORT" <>
     Opt.help "Use netstrings over a socket on PORT to communicate"))
  <|> pure Nothing


selectHost :: Maybe HostName -> HostName
selectHost (Just name) = name
selectHost Nothing     = "::1"

data SessionResult
  = MakeNew Port
  | MakeNewDyn (Port -> IO ())
  | UseExisting Port
  | MismatchesExisting Port Port

getOrLockSession :: Maybe Session -> Maybe _Publicity -> Maybe Port -> IO SessionResult
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

realMain :: App s -> ProgramMode -> IO ()
realMain theApp progMode =
  case progMode of
    StdIONetstring ->
      serveStdIONS theApp
    SocketNetstring (NetworkOptions session publicity port) ->
      do sessionResult <- getOrLockSession session publicity port
         hSetBuffering stdout NoBuffering
         case sessionResult of
           UseExisting (Port port) ->
             putStrLn ("PORT " ++ port)
           MakeNew (Port port) ->
             do putStrLn ("PORT " ++ port)
                serveSocket (selectHost publicity) port theApp
           MakeNewDyn registerPort ->
             do let h = selectHost publicity
                (a, port) <- serveSocketDynamic h theApp
                registerPort (Port (show port))
                putStrLn ("PORT " ++ show port)
                wait a
           MismatchesExisting (Port existingPort) (Port desiredPort) ->
             die $ "Named session already exists, but is running on port "
                   <> existingPort
                   <> ", not the specified port "
                   <> desiredPort
    Http path (NetworkOptions session publicity port) ->
      serveHTTP path theApp (maybe 8080 (read . unPort) port)
