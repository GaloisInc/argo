{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.Exceptions (cryptolParseErr, evalPolyErr, proverError) where

import Data.Aeson as JSON

import CryptolServer
import Argo


cryptolParseErr ::
  (ToJSON expr, Show err) =>
  expr {- ^ the input that couldn't be parsed -} ->
  err {- ^ the parse error from Cryptol -} ->
  JSONRPCException
cryptolParseErr expr err =
  makeJSONRPCException
    20100 "There was a Cryptol parse error."
    (Just $ JSON.object ["input" .= expr, "error" .= show err])

evalPolyErr ::
  (ToJSON ty) =>
  ty {- ^ the type that was too polymorphic -} ->
  JSONRPCException
evalPolyErr ty =
  makeJSONRPCException 20101 "Can't evaluate at polymorphic type" (Just (JSON.object ["type" .= ty]))

proverError :: String -> JSONRPCException
proverError msg = makeJSONRPCException 20102 "Prover error" (Just (JSON.toJSON msg))
