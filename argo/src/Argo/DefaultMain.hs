{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}
module Argo.DefaultMain (defaultMain) where


import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Foldable
import qualified Data.Text.IO as T
import Control.Concurrent.Async (wait)
import Control.Exception (handle, IOException)
import Network.Socket (HostName)
import qualified Options.Applicative as Opt
import Safe (readMay)
import System.IO (BufferMode(..), hPutStrLn, hSetBuffering, stderr, stdout)
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

-- | Options that are common to the network server modes of operation,
-- like socket and HTTP.
data NetworkOptions =
  NetworkOptions
  { networkSession :: Maybe Session
    -- ^ The name of an existing session to re-establish
  , networkHost :: Maybe HostName
    -- ^ The hostname on which to listen (e.g. "::" or "0.0.0.0" for
    -- public services)
  , networkPort :: Maybe Port
    -- ^ The port number on which to listen
  , networkLog :: Maybe LogOption
    -- ^ How to log incoming connections and messages
  }

data LogOption = StdErrLog

data ProgramMode
  = StdIONetstring (Maybe LogOption)
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
  Opt.info (StdIONetstring <$> logOpt) $
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
  NetworkOptions <$> sessionOpt <*> hostOpt <*> portOpt <*> logOpt

options :: String -> Opt.ParserInfo ProgramMode
options desc =
  Opt.info (mode desc <**> Opt.helper) $
  Opt.fullDesc <> Opt.progDesc desc

hostOpt :: Opt.Parser (Maybe HostName)
hostOpt =
  (fmap Just . Opt.strOption $
   Opt.long "host" <>
   Opt.metavar "HOSTNAME" <>
   Opt.help "Make server available at the specified hostname (use 0.0.0.0 or :: for publicly available services).")
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

logOpt :: Opt.Parser (Maybe LogOption)
logOpt =
  (Just <$>
   Opt.option whichLog
   (Opt.long "log" <>
    Opt.metavar "DEST" <>
    Opt.help "Output logs. Destination is 'stderr' for stderr."))
  <|> pure Nothing
  where
    whichLog =
      Opt.eitherReader $
        \case
          "stderr" -> Right StdErrLog
          other -> Left $ "Unknown log option '" ++ other ++ "'"

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
    StdIONetstring logging ->
      let logger = maybeLog logging
      in serveStdIONS logger theApp
    SocketNetstring (NetworkOptions session hostName port logging) ->
      do sessionResult <- getOrLockSession session hostName port
         let hostname = fromMaybe (selectHost hostName) hostName
         let logger = maybeLog logging
         hSetBuffering stdout NoBuffering
         case sessionResult of
           UseExisting (Port port) ->
             putStrLn ("PORT " ++ port)
           MakeNew (Port port) ->
             do putStrLn ("PORT " ++ port)
                serveSocket logger hostname port theApp
           MakeNewDyn registerPort ->
             do (a, port) <- serveSocketDynamic logger hostname theApp
                registerPort (Port (show port))
                putStrLn ("PORT " ++ show port)
                wait a
           MismatchesExisting (Port existingPort) (Port desiredPort) ->
             die $ "Named session already exists, but is running on port "
                   <> existingPort
                   <> ", not the specified port "
                   <> desiredPort
    Http path (NetworkOptions session _host port logging) ->
      case session of
        Just _ -> die "Named sessions not yet supported for HTTP"
        Nothing ->
          do let logger = maybeLog logging
             serveHttp logger path theApp (maybe 8080 (read . unPort) port)
  where maybeLog Nothing _ = pure ()
        maybeLog (Just StdErrLog) txt = T.hPutStrLn stderr txt
