{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Aeson as JSON (fromJSON, toJSON, Result(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM

import Test.QuickCheck.Instances.ByteString
import Test.QuickCheck.Instances.Scientific
import Test.QuickCheck.Instances.Text
import Test.Tasty
import Test.Tasty.QuickCheck

import JSONRPC
import Netstrings
import CryptolServer.Call

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "The tests" [ netstringProps, jsonRPCProps, callMsgProps ]

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

instance Arbitrary Encoding where
  arbitrary = oneof [pure Hex, pure Base64]

instance Arbitrary ArgSpec where
  arbitrary = sized spec
    where
      spec n
        | n <= 0 =
          oneof [ Bit <$> arbitrary
                , pure Unit
                , Num <$> arbitrary <*> arbitrary <*> arbitrary
                ]
        | otherwise =
          choose (0, n) >>=
          \len ->
            let sub = n `div` len
            in
              oneof [ Record . HM.fromList <$> vectorOf len ((,) <$> arbitrary <*> spec sub)
                    , Sequence <$> vectorOf len (spec sub)
                    , Tuple <$> vectorOf len (spec sub)
                    ]

callMsgProps :: TestTree
callMsgProps =
  testGroup "QuickCheck properties for the \"call\" message"
    [ testProperty "encoding and decoding arg specs is the identity" $
      \(spec :: ArgSpec) ->
        case fromJSON (toJSON spec) of
          JSON.Success v -> spec == v
          JSON.Error err -> False
    ]
