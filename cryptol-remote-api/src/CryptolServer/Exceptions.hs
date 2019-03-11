{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.Exceptions where

import Data.Aeson as JSON

import CryptolServer
import Argo.JSONRPC


cryptolParseErr ::
  (ToJSON expr, Show err) =>
  expr {- ^ the input that couldn't be parsed -} ->
  err {- ^ the parse error from Cryptol -} ->
  CryptolServerException
cryptolParseErr expr err rid =
  JSONRPCException { errorCode = 4
                   , message = "There was a Cryptol parse error."
                   , errorData = Just $ JSON.object ["input" .= expr, "error" .= show err]
                   , errorID = Just rid
                   }
