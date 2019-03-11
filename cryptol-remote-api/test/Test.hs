{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Aeson as JSON (fromJSON, toJSON, Result(..))

import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty(NonEmpty(..))

import Test.QuickCheck.Instances.ByteString
import Test.QuickCheck.Instances.Scientific
import Test.QuickCheck.Instances.Text
import Test.Tasty
import Test.Tasty.QuickCheck

import Argo.JSONRPC
import Argo.Netstring
import CryptolServer.Call

import Debug.Trace

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "The tests" [ callMsgProps ]


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
                , Integer <$> arbitrary
                -- NB: The following case will not generate
                -- syntactically valid Cryptol. But for testing
                -- round-tripping of the JSON, and coverage of various
                -- functions, it's better than nothing.
                , Concrete <$> arbitrary
                ]
        | otherwise =
          choose (2, n) >>=
          \len ->
            let sub = n `div` len
            in
              oneof [ Record . HM.fromList <$> vectorOf len ((,) <$> arbitrary <*> spec sub)
                    , Sequence <$> vectorOf len (spec sub)
                    , Tuple <$> vectorOf len (spec sub)
                    -- NB: Will not make valid identifiers, so if we
                    -- ever insert validation, then this will need to
                    -- change.
                    , Let <$> vectorOf len (LetBinding <$> arbitrary <*> spec sub) <*> spec sub
                    , Application <$> spec sub <*> ((:|) <$> spec sub <*> vectorOf len (spec sub))
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
