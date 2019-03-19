module Test.Data.Expr (suite) where

import Prelude

import Data.Argonaut (Json, decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (Either(..), either)
import Data.String (trim)
import Lynx.Data.Expr (Expr, ExprType, decodeExprTypeF, encodeExprTypeF)
import Matryoshka (anaM, cata, embed)
import Test.QuickCheck (Result(..), (===))
import Test.Unit (Test, TestSuite, failure, success, test)
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck (quickCheck)

suite :: TestSuite
suite = Test.Unit.suite "Test.Data.Expr" do
  Test.Unit.suite "ExprType" do
    test "decoding and encoding roundtrips properly" do
      quickCheck (exprTypeRoundTrip <<< embed)
  Test.Unit.suite "Expr" do
    test "JSON parses to an Equal" (assertRight testAEither)
    test "JSON parses to an If" (assertRight testBEither)
    test "decoding and encoding roundtrips properly" do
      quickCheck exprRoundTrip

exprTypeRoundTrip :: ExprType -> Result
exprTypeRoundTrip = roundTrip (anaM decodeExprTypeF) (cata encodeExprTypeF)

exprRoundTrip :: Expr -> Result
exprRoundTrip = roundTrip decodeJson encodeJson

roundTrip :: ∀ a. Eq a => Show a => (Json -> Either String a) -> (a -> Json) -> a -> Result
roundTrip decode encode x' = case decode json of
  Left error -> Failed $ show { encodedValue: stringify json, error, value: x' }
  Right x -> x === x'
  where
  json :: Json
  json = encode x'


assertRight :: forall a. Either String a -> Test
assertRight = either failure (const success)

test1Json :: String
test1Json = """
  { "op": "Val", "param": 1, "in": "Void", "out": "Int" }
"""

test2Json :: String
test2Json = """
  { "op": "Val", "param": 2, "in": "Void", "out": "Int" }
"""

testTrueJson :: String
testTrueJson = """
  { "op": "Val", "param": true, "in": "Void", "out": "Boolean" }
"""

testFalseJson :: String
testFalseJson = """
  { "op": "Val", "param": false, "in": "Void", "out": "Boolean" }
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
