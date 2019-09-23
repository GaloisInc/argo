{-# LANGUAGE OverloadedStrings #-}
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
import System.IO (BufferMode(..), hSetBuffering, stdout)

import Argo
import Argo.Socket
import Argo.HistoryWrapper
import Argo.CacheTree

import SAWServer
import SAWServer.CryptolSetup
import SAWServer.LLVMCrucibleSetup
import SAWServer.LLVMVerify
import SAWServer.SaveTerm


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
     theApp <- mkApp (HistoryWrapper cache) (historyWrapper validateSAWState sawMethods)
     case transportOpt opts of
       StdIONetstring -> serveStdIONS theApp
       SocketNetstring (Port p) -> serveSocket "127.0.0.1" p theApp
       SocketNetstringDyn h ->
         do hSetBuffering stdout NoBuffering
            (a, p) <- serveSocketDynamic h theApp
            putStrLn ("PORT " ++ show p)
            wait a

sawMethods :: [(Text, MethodType, JSON.Value -> Method SAWState JSON.Value)]
sawMethods =
  [ ("SAW/Cryptol/start setup",  Command, method startCryptolSetup)
  , ("SAW/Cryptol/load module",  Command, method cryptolSetupLoadModule)
  , ("SAW/Cryptol/load file",    Command, method cryptolSetupLoadFile)
  , ("SAW/Cryptol/finish setup", Command, method cryptolSetupDone)
  , ("SAW/Cryptol/save term",    Command, method saveTerm)
  , ("SAW/LLVM/start setup",     Command, method startLLVMCrucibleSetup)
  , ("SAW/LLVM/return",          Command, method llvmCrucibleReturn)
  , ("SAW/LLVM/finish setup",    Command, method llvmCrucibleSetupDone)
  , ("SAW/LLVM/load module",     Command, method llvmLoadModule)
  , ("SAW/LLVM/verify",          Command, method llvmVerify)
  ]
