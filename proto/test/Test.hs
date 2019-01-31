{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Aeson as JSON (fromJSON, toJSON, Result(..))


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
      \ bs -> parseNetstring (encodeNetstring (netstring bs)) == (netstring bs, "")
    , testProperty "doubly encoding and decoding is identity, with no leftover at either step " $
      \ bs -> let encode = encodeNetstring . netstring
                  (once, rest) = parseNetstring (encode (encode bs))
              in rest == "" && parseNetstring (decodeNetstring once) == (netstring bs, "")
    , testProperty "decoding leaves the right amount behind" $
      \ bs rest ->
        parseNetstring (encodeNetstring (netstring bs) <> rest) == (netstring bs, rest)
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
