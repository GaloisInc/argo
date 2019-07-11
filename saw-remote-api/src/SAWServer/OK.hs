{-# LANGUAGE OverloadedStrings #-}
module SAWServer.OK where

import Data.Aeson

data OK = OK

ok :: Applicative f => f OK
ok = pure OK

instance ToJSON OK where
  toJSON OK = "ok"