module Test.Lynx.Data.Form (suite) where

import Prelude

import Data.Argonaut (decodeJson, jsonParser)
import Data.Either (Either, either)
import Lynx.Data.Expr (Expr)
import Lynx.Data.Form (Page)
import Test.Unit (Test, TestSuite, failure, success, test)
import Test.Unit as Test.Unit

suite :: TestSuite
suite =
  Test.Unit.suite "Test.Lynx.Data.Form" do
    test "JSON parses to an Expr" (assertRight testPageEither)

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

textInput :: String
textInput = """
  { "type": "Text"
  , "required": """ <> val true "Boolean" <> """
  , "placeholder": """ <> val "" "String" <> """
  , "value": { "value": null, "source": null }
  , "default": null
  , "maxLength": null
  , "minLength": null
  }
"""

toggleInput :: String
toggleInput = """
  { "type": "Toggle"
  , "value": { "value": null, "source": null }
  , "default": """ <> val false "Boolean" <> """
  }
"""

firstName :: String
firstName = testField "First Name" "Enter your first name" textInput

lastName :: String
lastName = testField "Last Name" "Enter your last name" textInput

active :: String
active = testField "Active" "Is user's account active" toggleInput

val :: ∀ a. Show a => a -> String -> String
val x o  = """
  { "op": "Val", "param": """ <> show x <> """, "in": "Void", "out": """ <> show o <> """ }
"""