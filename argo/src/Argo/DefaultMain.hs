{-# LANGUAGE OverloadedStrings #-}
module Argo.DefaultMain (defaultMain) where


import Control.Applicative
import Control.Concurrent.Async (wait)
import Network.Socket (HostName)
import qualified Options.Applicative as Opt
import System.Exit (die)
import System.IO (BufferMode(..), hSetBuffering, stdout)

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

newtype Port = Port String

data TransportOpt
  = StdIONetstring
  -- ^ NetStrings over standard IO
  | SocketOpt Newness (Maybe Publicity) SocketOpt
  -- ^ NetStrings over some socket

data SocketOpt
  = SocketNetstring Port         -- ^ NetStrings over specific port (all hosts)
  | SocketNetstringDyn IPVersion -- ^ NetStrings over specific IP version (dynamic port)

data Publicity
  = Public

data Newness
  = StartNew
  | UseExisting

data IPVersion
  = IPv6 | IPv4

options :: String -> Opt.ParserInfo Options
options desc =
  Opt.info ((Options <$> transport) <**> Opt.helper) $
  Opt.fullDesc <> Opt.progDesc desc

transport :: Opt.Parser TransportOpt
transport =
  stdio <|>
  ((\s n p -> SocketOpt n p s) <$>
   (dyn4 <|> dyn6 <|> socket) <*> newness <*> publicity)
  <|> pure StdIONetstring
  where
    socket = SocketNetstring . Port <$>
             Opt.strOption (Opt.long "socket" <>
                            Opt.metavar "PORT" <>
                            Opt.help "Use netstrings over a socket on PORT to communicate")

    stdio  = Opt.flag' StdIONetstring $
             Opt.long "stdio" <> Opt.help "Use netstrings over stdio to communicate"

    dyn4   = Opt.flag' (SocketNetstringDyn IPv4) $
             Opt.long "dynamic4" <>
             Opt.help "Dynamically choose an IPv4 port on which to communicate using netstrings, and write it to stdout."

    dyn6   = Opt.flag' (SocketNetstringDyn IPv6) $
             Opt.long "dynamic" <>
             Opt.help "Dynamically choose an IPv6 port on which to communicate using netstrings, and write it to stdout."

publicity :: Opt.Parser (Maybe Publicity)
publicity =
  Opt.flag Nothing (Just Public) $
  Opt.long "public" <> Opt.help "Run the server publicly"

newness :: Opt.Parser Newness
newness =
  Opt.flag UseExisting StartNew $
  Opt.long "new" <>
  Opt.help "Start a new server process even if one is already running"

selectHost :: IPVersion -> Maybe Publicity -> HostName
selectHost IPv6 Nothing = "::1"
selectHost IPv4 Nothing = "127.0.0.1"
selectHost IPv6 (Just Public) = "::"
selectHost IPv4 (Just Public) = "0.0.0.0"

realMain :: App s -> Options -> IO ()
realMain theApp opts =
  case transportOpt opts of
    StdIONetstring ->
      serveStdIONS theApp
    SocketOpt newnessOpt publicityOpt socketOpt ->
      case socketOpt of
        SocketNetstring (Port p) ->
          serveSocket (selectHost IPv4 publicityOpt) p theApp
        SocketNetstringDyn ip ->
          do hSetBuffering stdout NoBuffering
             let h = selectHost ip publicityOpt
             (a, p) <- serveSocketDynamic h theApp
             putStrLn ("PORT " ++ show p)
             wait a
