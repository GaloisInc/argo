{-# LANGUAGE OverloadedStrings #-}
module SAWServer.Exceptions where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Argo

serverValNotFound ::
  (ToJSON name, Show name) =>
  name {- ^ the name that was not found -}->
  JSONRPCException
serverValNotFound name =
  makeJSONRPCException 1001 ("No server value with name " <> T.pack (show name))
    (Just $ object ["name" .= name])

-- TODO: make error say what kind of thing it was
notACryptolEnv ::
  (ToJSON name, Show name) =>
  name {- ^ the name that should have been mapped to a Cryptol environment -}->
  JSONRPCException
notACryptolEnv name =
  makeJSONRPCException 1002
    ("The server value with name " <>
     T.pack (show name) <>
     " is not a Cryptol environment")
    (Just $ object ["name" .= name])

notSettingUpCryptol :: JSONRPCException
notSettingUpCryptol = makeJSONRPCException 1003 "Not currently setting up Cryptol" noData
  where noData :: Maybe ()
        noData = Nothing

-- TODO: Get rid of these and make them specific
genericError :: Text -> JSONRPCException
genericError msg =
  makeJSONRPCException 100000 msg (Nothing :: Maybe ())
