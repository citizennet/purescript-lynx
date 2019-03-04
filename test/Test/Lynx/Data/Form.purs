module Test.Lynx.Data.Form (suite) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Lynx.Data.Expr (Expr)
import Lynx.Data.Form (Input, InputSource(..), Page)
import Test.QuickCheck (Result(..), (===))
import Test.Unit (Test, TestSuite, failure, success, test)
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck (quickCheck)

suite :: TestSuite
suite =
  Test.Unit.suite "Test.Lynx.Data.Form" do
    test "JSON parses to an Expr" (assertRight testPageEither)
    Test.Unit.suite "Input" do
      test "decoding and encoding roundtrips properly" do
        quickCheck inputRoundTrip
    Test.Unit.suite "InputSource" do
      test "decoding and encoding roundtrips properly" do
        quickCheck inputSourceRoundTrip

inputRoundTrip :: Input Expr -> Result
inputRoundTrip = roundTrip

inputSourceRoundTrip :: InputSource Int -> Result
inputSourceRoundTrip = roundTrip

roundTrip :: ∀ a. DecodeJson a => EncodeJson a => Eq a => Show a => a -> Result
roundTrip x' = case decodeJson json of
  Left error -> Failed $ show { encodedValue: stringify json, error, value: x' }
  Right x -> x === x'
  where
  json :: Json
  json = encodeJson x'

assertRight :: forall a. Either String a -> Test
assertRight = either failure (const success)

testPageEither :: Either String (Page Expr)
testPageEither = decodeJson =<< jsonParser testPageJson

testPageJson :: String
testPageJson = """
  { "name": "Profile"
  , "contents":
    [ """ <> testSection <> """
    ]
  }
"""

testSection :: String
testSection = """
  { "name": "Name"
  , "contents":
    [ """ <> firstName <> """
    , """ <> lastName <> """
    , """ <> active <> """
    ]
  }
"""

testField :: String -> String -> String -> String
testField n d i = """
  { "name": """ <> val n "String" <> """
  , "visibility": """ <> val true "Boolean" <> """
  , "description": """ <> val d "String" <> """
  , "key": "firstName"
  , "input": """ <> i <> """
  }
"""

textInput :: String -> String
textInput v = """
  { "type": "Text"
  , "required": """ <> val true "Boolean" <> """
  , "placeholder": """ <> val "" "String" <> """
  , "value": """ <> v <> """
  , "default": null
  , "maxLength": null
  , "minLength": null
  }
"""

toggleInput :: String -> String
toggleInput v = """
  { "type": "Toggle"
  , "value": """ <> v <> """
  , "default": """ <> val false "Boolean" <> """
  }
"""

firstName :: String
firstName =
  testField
    "First Name"
    "Enter your first name"
    (textInput $ value $ Just $ UserInput $ val "Pat" "String")

lastName :: String
lastName =
  testField
    "Last Name"
    "Enter your last name"
    (textInput $ value Nothing)

active :: String
active =
  testField
    "Active"
    "Is user's account active"
    (toggleInput $ value $ Just $ Invalid $ val 10 "Int")

val :: ∀ a. Show a => a -> String -> String
val x o  = """
  { "op": "Val", "param": """ <> show x <> """, "in": "Void", "out": """ <> show o <> """ }
"""

value :: Maybe (InputSource String) -> String
value = maybe "null" case _ of
  UserInput x -> """
      { "type": "UserInput", "value": """ <> x <> """ }
    """
  Invalid x -> """
      { "type": "Invalid", "value": """ <> x <> """ }
    """
