module Test.Data.Expr (suite) where

import Prelude

import Data.Argonaut (decodeJson, jsonParser)
import Data.Either (Either, either)
import Data.String (trim)
import Lynx.Data.Expr (Expr)
import Test.Unit (Test, TestSuite, failure, success, test)
import Test.Unit as Test.Unit

suite :: TestSuite
suite = Test.Unit.suite "Test.Data.Expr" do
  test "JSON parses to an Equal" (assertRight testAEither)
  test "JSON parses to an If" (assertRight testBEither)

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
