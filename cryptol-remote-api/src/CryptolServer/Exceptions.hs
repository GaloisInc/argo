{-# LANGUAGE OverloadedStrings #-}
module CryptolServer.Exceptions
  ( invalidBase64
  , invalidHex
  , invalidType
  , unwantedDefaults
  , evalInParamMod
  , evalPolyErr
  , proverError
  , cryptolParseErr
  ) where

import Data.Aeson as JSON hiding (Encoding, Value, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap

import Cryptol.ModuleSystem.Name (Name)
import Cryptol.Parser
import qualified Cryptol.TypeCheck.Type as TC
import Cryptol.Utils.PP (pretty)

import Argo
import CryptolServer.Data.Type

invalidBase64 :: ByteString -> String -> JSONRPCException
invalidBase64 invalidData msg =
  makeJSONRPCException
    20020 (T.pack msg)
    (Just (JSON.object ["input" .= B8.unpack invalidData]))

invalidHex :: Char -> JSONRPCException
invalidHex invalidData =
  makeJSONRPCException
    20030 "Not a hex digit"
    (Just (JSON.object ["input" .= T.singleton invalidData]))

invalidType :: TC.Type -> JSONRPCException
invalidType ty =
  makeJSONRPCException
    20040 "Can't convert Cryptol data from this type to JSON"
    (Just (jsonTypeAndString ty))

jsonTypeAndString :: TC.Type -> JSON.Object
jsonTypeAndString ty =
  HashMap.fromList
    [ "type" .= JSONSchema (TC.Forall [] [] ty)
    , "type string" .= pretty ty ]

unwantedDefaults :: [(TC.TParam, TC.Type)] -> JSONRPCException
unwantedDefaults defs =
  makeJSONRPCException
    20210 "Execution would have required these defaults"
    (Just (JSON.object ["defaults" .=
      [ jsonTypeAndString ty <> HashMap.fromList ["parameter" .= pretty param]
      | (param, ty) <- defs ] ]))

evalInParamMod :: [Cryptol.ModuleSystem.Name.Name] -> JSONRPCException
evalInParamMod mods =
  makeJSONRPCException
    20220 "Can't evaluate Cryptol in a parameterized module."
    (Just (JSON.object ["modules" .= map pretty mods]))

evalPolyErr ::
  TC.Schema {- ^ the type that was too polymorphic -} ->
  JSONRPCException
evalPolyErr schema =
  makeJSONRPCException
    20200 "Can't evaluate at polymorphic type"
    (Just (JSON.object [ "type" .= JSONSchema schema
                       , "type string" .= pretty schema ]))

proverError :: String -> JSONRPCException
proverError msg =
  makeJSONRPCException
    20230 "Prover error"
    (Just (JSON.object ["error" .= msg]))

cryptolParseErr ::
  Text {- ^ the input that couldn't be parsed -} ->
  ParseError {- ^ the parse error from Cryptol -} ->
  JSONRPCException
cryptolParseErr expr err =
  makeJSONRPCException
    20000 "Cryptol parse error"
    (Just $ JSON.object ["input" .= expr, "error" .= show err])
