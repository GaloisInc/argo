{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Aeson as JSON (fromJSON, toJSON, Result(..))
import Data.ByteString.Lazy (ByteString)

import Test.QuickCheck.Instances.ByteString
import Test.QuickCheck.Instances.Scientific
import Test.QuickCheck.Instances.Text
import Test.Tasty
import Test.Tasty.QuickCheck

import JSONRPC
import Netstrings


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "The tests" [ netstringProps, jsonRPCProps ]

netstringProps :: TestTree
netstringProps =
  testGroup "QuickCheck properties for netstrings"
    [ testProperty "fromNetstring . toNetstring is identity, with no leftover " $
      \ (bs :: ByteString) -> parseNetstring (encodeNetstring (netstring bs)) == (netstring bs, "")
    , testProperty "doubly encoding and decoding is identity, with no leftover at either step " $
      \ (bs :: ByteString) ->
        let (once, rest) = parseNetstring (encodeNetstring (netstring bs))
              in rest == "" && parseNetstring (encodeNetstring once) == (netstring bs, "")
    , testProperty "decoding leaves the right amount behind" $
      \ (bs :: ByteString) (rest :: ByteString) ->
        parseNetstring ((encodeNetstring (netstring bs)) <> rest) == (netstring bs, rest)
    ]

instance Arbitrary RequestID where
  arbitrary =
    oneof [ IDText <$> arbitrary
          , IDNum <$> arbitrary
          , pure IDNull
          ]


jsonRPCProps :: TestTree
jsonRPCProps =
  testGroup "QuickCheck properties for JSONRPC"
    [ testProperty "encoding and decoding request IDs is the identity" $
      \(rid :: RequestID) ->
        case fromJSON (toJSON rid) of
          JSON.Success v -> rid == v
          JSON.Error err -> False
    ]
