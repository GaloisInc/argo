{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.Exceptions where

import Data.Aeson as JSON

import CryptolServer
import Argo.JSONRPC


cryptolParseErr ::
  (ToJSON expr, Show err) =>
  expr {- ^ the input that couldn't be parsed -} ->
  err {- ^ the parse error from Cryptol -} ->
  JSONRPCException
cryptolParseErr expr err =
  makeJSONRPCException
    4 "There was a Cryptol parse error."
    (Just $ JSON.object ["input" .= expr, "error" .= show err])

evalPolyErr ::
  (ToJSON ty) =>
  ty {- ^ the type that was too polymorphic -} ->
  JSONRPCException
evalPolyErr ty =
  makeJSONRPCException 45 "Can't evaluate at polymorphic type" (Just (JSON.object ["type" .= ty]))
