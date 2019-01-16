> {-# LANGUAGE OverloadedStrings #-}
> module Main where

> import qualified Data.Aeson as JSON

> import JSONRPC

> import Cryptol.REPL.Monad

> main :: IO ()
> main =
>   serveStdIONS =<< mkApp (0 :: Integer) [("increment", Command $ \s params -> return (s + 1, JSON.toJSON (s + 1)))]
