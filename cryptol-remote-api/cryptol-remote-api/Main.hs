{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Concurrent.Async (wait)
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as JSON
import Data.Text (Text)
import qualified Options.Applicative as Opt
import System.Directory (doesDirectoryExist, setCurrentDirectory)
import System.IO (stdout)

import Argo
import Argo.Socket
import Argo.HistoryWrapper
import Argo.CacheTree

import CryptolServer
import CryptolServer.Call
import CryptolServer.ChangeDir
import CryptolServer.EvalExpr
import CryptolServer.FocusedModule
import CryptolServer.LoadModule
import CryptolServer.Names
import CryptolServer.Sat
import CryptolServer.TypeCheck



main :: IO ()
main =
  do opts <- Opt.execParser options
     realMain opts


data Options =
  Options
    { transportOpt :: TransportOpt
    }

newtype Port = Port String

data TransportOpt
  = StdIONetstring              -- ^ NetStrings over standard IO
  | SocketNetstring Port        -- ^ NetStrings over specific port (all hosts)
  | SocketNetstringDyn String   -- ^ NetStrings over specific host (dynamic port)

options :: Opt.ParserInfo Options
options = Opt.info (Options <$> transport) (Opt.fullDesc)

transport :: Opt.Parser TransportOpt
transport = dyn4 <|> dyn6 <|> socket <|> stdio <|> pure StdIONetstring
  where
    socket      = SocketNetstring . Port <$>
                    Opt.strOption (Opt.long "socket" <> Opt.metavar "PORT" )

    stdio       = Opt.flag' StdIONetstring (Opt.long "stdio" <> Opt.help "Use netstrings over stdio")

    dyn4        = Opt.flag' (SocketNetstringDyn "127.0.0.1") (Opt.long "dynamic4")

    dyn6        = Opt.flag' (SocketNetstringDyn "::1"      ) (Opt.long "dynamic")


realMain :: Options -> IO ()
realMain opts =
  do initSt <- initialState
     cache  <- newCache initSt
     theApp <- mkApp (HistoryWrapper cache) (historyWrapper validateServerState cryptolMethods)
     case transportOpt opts of
       StdIONetstring -> serveStdIONS theApp
       SocketNetstring (Port p) -> serveSocket "127.0.0.1" p theApp
       SocketNetstringDyn h ->
         do (a, p) <- serveSocketDynamic h theApp
            putStrLn ("PORT " ++ show p)
            wait a


cryptolMethods :: [(Text, MethodType, JSON.Value -> Method ServerState JSON.Value)]
cryptolMethods =
  [ ("change directory",    Command, method cd)
  , ("load module",         Command, method loadModule)
  , ("focused module",      Query,   method focusedModule)
  , ("evaluate expression", Query,   method evalExpression)
  , ("call",                Query,   method call)
  , ("visible names",       Query,   method visibleNames)
  , ("check type",          Query,   method checkType)
  , ("satisfy",             Query,   method sat)
  ]
