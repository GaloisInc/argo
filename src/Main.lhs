> module Main where

> import JSONRPC

> main :: IO ()
> main =
>   do putStrLn "Hello, Haskell!"
>      serveStdIO =<< mkApp () []
