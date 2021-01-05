{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}
module Argo.DefaultMain
  ( -- * Main functions
    defaultMain, customMain,
    -- * Options
    UserOptions(..),
    userOptions) where

import Control.Applicative
import Control.Monad
import Control.Lens (view)
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
import qualified Argo.Doc as Doc
import Argo.Doc.Protocol (protocolDocs)
import Argo.Doc.ReST
import Argo.Socket

-- | The options selected by a user, including the server mode but
-- excluding mode-specific initialization options (e.g. the HTTP port).
data UserOptions stdIOOpts socketOpts httpOpts docOpts
  = StdIOOpts stdIOOpts
  | SocketOpts socketOpts
  | HttpOpts httpOpts
  | DocOpts docOpts

-- | If all modes use the same user option, collapse them into a
-- single structure.
userOptions :: UserOptions a a a a -> a
userOptions (StdIOOpts opts) = opts
userOptions (SocketOpts opts) = opts
userOptions (HttpOpts opts) = opts
userOptions (DocOpts opts) = opts

-- | Run an Argo application using the default set of command-line
-- arguments and server modes, augmented with additional command-line
-- arguments for application-specific initialization.
--
-- For instance, this can be used to specify a file to load at server
-- startup, or to customize server settings. The available settings
-- can be different for each server mode, if desired.
customMain ::
  Opt.Parser stdIOOpts {- ^ A command-line parser for the options for stdio mode -} ->
  Opt.Parser socketOpts {- ^ A command-line parser for the options for socket mode -} ->
  Opt.Parser httpOpts {- ^ A command-line parser for the options for HTTP mode -} ->
  Opt.Parser docOpts {- ^ A command-line parser for the options for documenation -} ->
  String {- ^ A description to be shown to users in the --help text -} ->
  (UserOptions stdIOOpts socketOpts httpOpts docOpts -> IO (App s))
    {- ^ An initialization procedure for the application that transforms the
         parsed custom options into an application -} ->
  IO ()
customMain stdioOpts socketOpts httpOpts docOpts str app =
  do opts <- Opt.customExecParser
               (Opt.prefs $ Opt.showHelpOnError <> Opt.showHelpOnEmpty)
               (options stdioOpts socketOpts httpOpts docOpts str)
     realMain app opts

-- | Run an Argo application using the default set of command-line
-- arguments and server modes.
defaultMain ::
  String {- ^ A description to be shown to users in the --help text -} ->
  App s {- ^ The application to be run -} ->
  IO ()
defaultMain str app =
  customMain parseNoOpts parseNoOpts parseNoOpts parseNoOpts str $ const $ pure app

data NoOpts = NoOpts

parseNoOpts :: Opt.Parser NoOpts
parseNoOpts = pure NoOpts

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

data ProgramMode stdioOpts socketOpts httpOpts docOpts
  = StdIONetstring (Maybe LogOption) stdioOpts
  -- ^ NetStrings over standard IO
  | SocketNetstring NetworkOptions socketOpts
  -- ^ NetStrings over some socket
  | Http String NetworkOptions httpOpts
  -- ^ HTTP (String is path at which API is served)
  | Doc !Format docOpts
  -- ^ Dump protocol description to stdout

newtype Port = Port {unPort :: String} deriving (Eq, Ord)

newtype Session
  = Session String

data Format = ReST

mode ::
  Opt.Parser stdioOpts ->
  Opt.Parser socketOpts ->
  Opt.Parser httpOpts ->
  Opt.Parser docOpts ->
  String -> Opt.Parser (ProgramMode stdioOpts socketOpts httpOpts docOpts)
mode stdio socket http doc desc =
  Opt.hsubparser $ stdioMode stdio desc <> socketMode socket desc <> httpMode http desc <> docMode doc desc

stdioMode :: Opt.Parser stdioOpts -> String -> Opt.Mod Opt.CommandFields (ProgramMode stdioOpts socketOpts httpOpts docOpts)
stdioMode user desc = Opt.command "stdio" $
  Opt.info (StdIONetstring <$> logOpt <*> user) $
  Opt.header desc <> Opt.fullDesc <> Opt.progDesc "Communicate over stdin and stdout"

socketMode :: Opt.Parser socketOpts -> String -> Opt.Mod Opt.CommandFields (ProgramMode stdioOpts socketOpts httpOpts docOpts)
socketMode user desc = Opt.command "socket" $
  Opt.info (SocketNetstring <$> networkOptions <*> user) $
  Opt.header desc <> Opt.fullDesc <> Opt.progDesc "Communicate over a socket"

httpMode :: Opt.Parser httpOpts -> String -> Opt.Mod Opt.CommandFields (ProgramMode stdioOpts socketOpts httpOpts docOpts)
httpMode user desc = Opt.command "http" $
  Opt.info (Http <$> path <*> networkOptions <*> user) $
  Opt.header desc <> Opt.fullDesc <> Opt.progDesc "Communicate over HTTP"
  where
    path =
      Opt.argument Opt.str $
      Opt.metavar "PATH" <> Opt.help "The path at which to serve the API"

docMode :: Opt.Parser docOpts -> String -> Opt.Mod Opt.CommandFields (ProgramMode stdioOpts socketOpts httpOpts docOpts)
docMode user desc = Opt.command "doc" $
  Opt.info (Doc <$> format <*> user) $
  Opt.header desc <> Opt.fullDesc <> Opt.progDesc "Emit protocol documentation"
  where
    format = pure ReST


networkOptions :: Opt.Parser NetworkOptions
networkOptions =
  NetworkOptions <$> sessionOpt <*> hostOpt <*> portOpt <*> logOpt

options ::
  Opt.Parser stdioOpts ->
  Opt.Parser socketOpts ->
  Opt.Parser httpOpts ->
  Opt.Parser docOpts ->
  String ->
  Opt.ParserInfo (ProgramMode stdioOpts socketOpts httpOpts docOpts)
options stdioOpts socketOpts httpOpts docOpts desc =
  Opt.info (mode stdioOpts socketOpts httpOpts docOpts desc <**> Opt.helper) $
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

realMain ::
  (UserOptions stdIOOpts socketOpts httpOpts docOpts -> IO (App s)) ->
  ProgramMode stdIOOpts socketOpts httpOpts docOpts -> IO ()
realMain makeApp progMode =
  case progMode of
    StdIONetstring logging userOpts ->
      do theApp <- makeApp (StdIOOpts userOpts)
         let logger = maybeLog logging
         serveStdIONS logger theApp
    SocketNetstring (NetworkOptions session hostName port logging) userOpts ->
      do theApp <- makeApp (SocketOpts userOpts)
         sessionResult <- getOrLockSession session hostName port
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
    Http path (NetworkOptions session _host port logging) userOpts ->
      do theApp <- makeApp (HttpOpts userOpts)
         case session of
           Just _ -> die "Named sessions not yet supported for HTTP"
           Nothing ->
             do let logger = maybeLog logging
                serveHttp logger path theApp (maybe 8080 (read . unPort) port)
    Doc format userOpts ->
      case format of
        ReST ->
          do theApp <- makeApp (DocOpts userOpts)
             T.putStrLn $
               restructuredText $
                 Doc.App (view appName theApp) (protocolDocs : view appDocumentation theApp)

  where maybeLog Nothing _ = pure ()
        maybeLog (Just StdErrLog) txt = T.hPutStrLn stderr txt
