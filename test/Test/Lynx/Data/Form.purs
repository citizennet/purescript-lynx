module Test.Lynx.Data.Form (suite) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (Either(..), either)
import Data.Foldable (findMap)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Lynx.Data.Expr (EvalError, Expr, ExprType(..), Key, boolean_, if_, lookup_, string_, val_)
import Lynx.Data.Form (Field, Input(..), InputSource(..), Page, Section)
import Lynx.Data.Form as Lynx.Data.Form
import Test.QuickCheck (Result(..), (===))
import Test.Unit (Test, TestSuite, failure, success, test)
import Test.Unit as Test.Unit
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

suite :: TestSuite
suite =
  Test.Unit.suite "Test.Lynx.Data.Form" do
    Test.Unit.suite "Page" do
      test "JSON parses to an Expr" do
        assertRight testPageEither
      test "decoding and encoding roundtrips properly" do
        quickCheck pageRoundTrip
      Test.Unit.suite "dropdown options can be dynamic" do
        dropdownOptions
    Test.Unit.suite "Section" do
      test "decoding and encoding roundtrips properly" do
        quickCheck sectionRoundTrip
    Test.Unit.suite "Field" do
      test "decoding and encoding roundtrips properly" do
        quickCheck fieldRoundTrip
    Test.Unit.suite "Input" do
      test "decoding and encoding roundtrips properly" do
        quickCheck inputRoundTrip
    Test.Unit.suite "InputSource" do
      test "decoding and encoding roundtrips properly" do
        quickCheck inputSourceRoundTrip

dropdownOptions :: TestSuite
dropdownOptions = do
  let evaluated' :: Either EvalError (Page ExprType)
      evaluated' = Lynx.Data.Form.eval (\key -> Data.Map.lookup key keys') page'
      keys' :: Map Key ExprType
      keys' = Lynx.Data.Form.keys page'

  test "initial lookup" do
    let actual :: Maybe ExprType
        actual = findOptions evaluated'
        expected :: ExprType
        expected = Array []
    equal (Just expected) actual

  let evaluated :: Either EvalError (Page ExprType)
      evaluated = Lynx.Data.Form.eval (\key -> Data.Map.lookup key keys) page
      keys :: Map Key ExprType
      keys = Lynx.Data.Form.keys page
      page :: Page Expr
      page = Lynx.Data.Form.setValue fooKey (UserInput $ Boolean true) page'

  test "after altering the toggle" do
    let actual :: Maybe ExprType
        actual = findOptions evaluated
        expected :: ExprType
        expected = Array [Pair { name: String "foo", value: Int 3}]
    equal (Just expected) actual

  where
  dropdown :: Input Expr
  dropdown =
    Dropdown
      { default: Nothing
      , options:
        if_ (lookup_ fooKey $ val_ (boolean_ false))
        (val_ $ Array [Pair { name: String "foo", value: Int 3}])
        (val_ $ Array [])
      , placeholder: val_ (string_ "")
      , required: val_ (boolean_ false)
      , value: NotSet
      }
  dropdownKey :: Key
  dropdownKey = "dropdown"
  findOptions :: forall a b. Either a (Page b) -> Maybe b
  findOptions = findMap \evaluatedPage ->
    flip findMap evaluatedPage.contents \section ->
      flip findMap section.contents \field ->
        if field.key == dropdownKey then
          case field.input of
            Dropdown x -> Just x.options
            _ -> Nothing
        else
          Nothing
  foo :: Input Expr
  foo =
    Toggle
      { default: Nothing
      , value: NotSet
      }
  fooKey :: Key
  fooKey = "foo"
  page' :: Page Expr
  page' =
    { name: ""
    , contents:
      [ { name: ""
        , contents:
          [ { name: val_ (string_ "")
            , visibility: val_ (boolean_ false)
            , description: val_ (string_ "")
            , key: fooKey
            , input: foo
            }
          , { name: val_ (string_ "")
            , visibility: val_ (boolean_ false)
            , description: val_ (string_ "")
            , key: dropdownKey
            , input: dropdown
            }
          ]
        }
      ]
    }


pageRoundTrip :: Page Expr -> Result
pageRoundTrip = roundTrip

sectionRoundTrip :: Section Expr -> Result
sectionRoundTrip = roundTrip

fieldRoundTrip :: Field Expr -> Result
fieldRoundTrip = roundTrip

inputRoundTrip :: Input Expr -> Result
inputRoundTrip = roundTrip

inputSourceRoundTrip :: InputSource Expr -> Result
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
    (textInput $ value $ UserInput $ val "Pat" "String")

lastName :: String
lastName =
  testField
    "Last Name"
    "Enter your last name"
    (textInput $ value NotSet)

active :: String
active =
  testField
    "Active"
    "Is user's account active"
    (toggleInput $ value $ Invalid $ val 10 "Int")

val :: ∀ a. Show a => a -> String -> String
val x o  = """
  { "op": "Val", "param": """ <> show x <> """, "in": "Void", "out": """ <> show o <> """ }
"""

value :: InputSource String -> String
value = case _ of
  UserInput x -> """
      { "type": "UserInput", "value": """ <> x <> """ }
    """
  Invalid x -> """
      { "type": "Invalid", "value": """ <> x <> """ }
    """
  NotSet -> """
      { "type": "NotSet" }
    """
  UserCleared -> """
      { "type": "UserCleared" }
    """
