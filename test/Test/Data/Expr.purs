module Test.Data.Expr (suite) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (Either(..), either)
import Data.String (trim)
import Lynx.Data.Expr (Expr)
import Test.QuickCheck (Result(..), (===))
import Test.Unit (Test, TestSuite, failure, success, test)
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck (quickCheck)

suite :: TestSuite
suite = Test.Unit.suite "Test.Data.Expr" do
  Test.Unit.suite "Expr" do
    test "JSON parses to an Equal" (assertRight testAEither)
    test "JSON parses to an If" (assertRight testBEither)
    test "decoding and encoding roundtrips properly" do
      quickCheck exprRoundTrip

exprRoundTrip :: Expr -> Result
exprRoundTrip = roundTrip

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

test1Either :: Either String Expr
test1Either = decodeJson =<< jsonParser test1Json

testAJson :: String
testAJson = """
  { "op": "Equal"
  , "params":
    [ """ <> trim test1Json <> """
    , """ <> trim test2Json <> """
    ]
  , "in": "Int"
  , "out": "Boolean"
  }
"""

testAEither :: Either String Expr
testAEither = decodeJson =<< jsonParser testAJson

testBJson :: String
testBJson = """
  { "op": "If"
  , "params":
    [ """ <> trim testAJson <> """
    , """ <> trim test1Json <> """
    , """ <> trim test2Json <> """
    ]
  , "in": "Boolean"
  , "out": "Int"
  }
"""

testBEither :: Either String Expr
testBEither = decodeJson =<< jsonParser testBJson
