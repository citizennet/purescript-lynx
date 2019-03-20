module Test.Lynx.Data.ExprType (suite) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (Either(..), either)
import Lynx.Data.ExprType (ExprType)
import Test.QuickCheck (Result(..), (===))
import Test.Unit (Test, TestSuite, failure, success, test)
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck (quickCheck)

suite :: TestSuite
suite = Test.Unit.suite "Test.Lynx.Data.ExprType" do
  Test.Unit.suite "ExprType" do
    test "decoding and encoding roundtrips properly" do
      quickCheck exprTypeRoundTrip
    test "JSON parses to 1" (assertRight test1Either)
    test "JSON parses to 2" (assertRight test2Either)
    test "JSON parses to true" (assertRight testTrueEither)
    test "JSON parses to false" (assertRight testFalseEither)

exprTypeRoundTrip :: ExprType -> Result
exprTypeRoundTrip = roundTrip

roundTrip :: ∀ a. DecodeJson a => EncodeJson a => Eq a => Show a => a -> Result
roundTrip x' = case decodeJson json of
  Left error -> Failed $ show { encodedValue: stringify json, error, value: x' }
  Right x -> x === x'
  where
  json :: Json
  json = encodeJson x'

assertRight :: forall a. Either String a -> Test
assertRight = either failure (const success)

test1Json :: String
test1Json = """
  { "op": "Val", "params": [1], "in": "Void", "out": "Int" }
"""

test2Json :: String
test2Json = """
  { "op": "Val", "params": [2], "in": "Void", "out": "Int" }
"""

testTrueJson :: String
testTrueJson = """
  { "op": "Val", "params": [true], "in": "Void", "out": "Boolean" }
"""

testFalseJson :: String
testFalseJson = """
  { "op": "Val", "params": [false], "in": "Void", "out": "Boolean" }
"""

test1Either :: Either String ExprType
test1Either = decodeJson =<< jsonParser test1Json

test2Either :: Either String ExprType
test2Either = decodeJson =<< jsonParser test2Json

testTrueEither :: Either String ExprType
testTrueEither = decodeJson =<< jsonParser testTrueJson

testFalseEither :: Either String ExprType
testFalseEither = decodeJson =<< jsonParser testFalseJson
