module Test.Lynx.Form (suite) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (Either(..), either)
import Data.Foldable (findMap)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty as Data.NonEmpty
import Lynx.Expr (EvalError, Expr, ExprType, Key, array_, boolean_, if_, int_, lookup_, pair_, string_, val_)
import Lynx.Form (Errors, Field, Input(..), InputSource(..), Page, Section, Tab, TabSections(..), TemplateInput, ValidationError)
import Lynx.Form as Lynx.Form
import Test.QuickCheck (Result(..), (===))
import Test.Unit (Test, TestSuite, failure, success, test)
import Test.Unit as Test.Unit
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck, quickCheck')

suite :: TestSuite
suite =
  Test.Unit.suite "Test.Lynx.Form" do
    Test.Unit.suite "Page" do
      test "JSON parses to an Expr" do
        assertRight testPageEither
      test "decoding and encoding roundtrips properly" do
        -- We don't need a large landscape of examples,
        -- most of the important logic is tested in other places.
        -- If we use the default 100 examples, it takes far too long.
        -- Generate a handful as a sanity check.
        quickCheck' 3 pageRoundTrip
      Test.Unit.suite "dropdown options can be dynamic" do
        dropdownOptions
    Test.Unit.suite "Tab" do
      test "decoding and encoding roundtrips properly" do
        quickCheck' 3 tabRoundTrip
    Test.Unit.suite "TabSections" do
      test "decoding and encoding roundtrips properly" do
        quickCheck' 3 tabSectionsRoundTrip
    Test.Unit.suite "TemplateInput" do
      test "decoding and encoding roundtrips properly" do
        quickCheck templateInputRoundTrip
    Test.Unit.suite "Field" do
      test "decoding and encoding roundtrips properly" do
        quickCheck' 3 fieldRoundTrip
    Test.Unit.suite "Input" do
      test "decoding and encoding roundtrips properly" do
        quickCheck inputRoundTrip
    Test.Unit.suite "InputSource" do
      test "decoding and encoding roundtrips properly" do
        quickCheck inputSourceRoundTrip
    Test.Unit.suite "Errors" do
      test "decoding and encoding roundtrips properly" do
        quickCheck errorsRoundTrip
    Test.Unit.suite "ValidationError" do
      test "decoding and encoding roundtrips properly" do
        quickCheck validationErrorRoundTrip

dropdownOptions :: TestSuite
dropdownOptions = do
  let evaluated' :: Either EvalError (Page ExprType)
      evaluated' = Lynx.Form.eval (\key -> Data.Map.lookup key keys') page'
      keys' :: Map Key ExprType
      keys' = Lynx.Form.keys page'

  test "initial lookup" do
    let actual :: Maybe ExprType
        actual = findOptions evaluated'
        expected :: ExprType
        expected = array_ []
    equal (Just expected) actual

  let evaluated :: Either EvalError (Page ExprType)
      evaluated = Lynx.Form.eval (\key -> Data.Map.lookup key keys) page
      keys :: Map Key ExprType
      keys = Lynx.Form.keys page
      page :: Page Expr
      page = Lynx.Form.setValue fooKey (UserInput $ boolean_ true) page'

  test "after altering the toggle" do
    let actual :: Maybe ExprType
        actual = findOptions evaluated
        expected :: ExprType
        expected = array_ [pair_ { name: string_ "foo", value: int_ 3}]
    equal (Just expected) actual

  where
  dropdown :: Input Expr
  dropdown =
    Dropdown
      { default: Nothing
      , options:
        if_ (lookup_ fooKey $ val_ (boolean_ false))
        (val_ $ array_ [pair_ { name: string_ "foo", value: int_ 3}])
        (val_ $ array_ [])
      , placeholder: val_ (string_ "")
      , required: val_ (boolean_ false)
      , value: NotSet
      , errors: mempty
      }
  dropdownKey :: Key
  dropdownKey = "dropdown"
  findOptions :: forall a b. Either a (Page b) -> Maybe b
  findOptions = findMap \evaluatedPage ->
    flip findMap evaluatedPage.tabs \tab ->
      flip findMap tab.sections \sections' ->
        case sections' of
          TabSection section -> getOption section
          TabSequence sequence -> case sequence.values of
            UserInput sections -> flip findMap sections getOption
            Invalid sections -> flip findMap sections getOption
            _ -> Nothing

  getOption :: forall a. Section a -> Maybe a
  getOption section = flip findMap section.fields \field ->
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
      , errors: mempty
      }
  fooKey :: Key
  fooKey = "foo"
  page' :: Page Expr
  page' =
    { name: ""
    , tabs:
      Data.NonEmpty.singleton
        { name: ""
        , link: ""
        , sections: Data.NonEmpty.singleton $
            TabSection
              { name: ""
              , fields:
                Data.NonEmpty.NonEmpty
                { name: val_ (string_ "")
                , visibility: val_ (boolean_ false)
                , description: val_ (string_ "")
                , key: fooKey
                , input: foo
                }
                [ { name: val_ (string_ "")
                  , visibility: val_ (boolean_ false)
                  , description: val_ (string_ "")
                  , key: dropdownKey
                  , input: dropdown
                  }
                ]
              }
        }
    }

pageRoundTrip :: Page Expr -> Result
pageRoundTrip = roundTrip

tabRoundTrip :: Tab Expr -> Result
tabRoundTrip = roundTrip

tabSectionsRoundTrip :: TabSections Expr -> Result
tabSectionsRoundTrip = roundTrip

templateInputRoundTrip :: TemplateInput Expr -> Result
templateInputRoundTrip = roundTrip

fieldRoundTrip :: Field Expr -> Result
fieldRoundTrip = roundTrip

inputRoundTrip :: Input Expr -> Result
inputRoundTrip = roundTrip

inputSourceRoundTrip :: InputSource Expr -> Result
inputSourceRoundTrip = roundTrip

errorsRoundTrip :: Errors ValidationError -> Result
errorsRoundTrip = roundTrip

validationErrorRoundTrip :: ValidationError -> Result
validationErrorRoundTrip = roundTrip

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
  , "tabs":
    [ """ <> testTab <> """
    ]
  }
"""

testTab :: String
testTab = """
  { "name": "User"
  , "link": "user"
  , "sections":
    [ """ <> testSection <> """
    , """ <> testSequence <> """
    ]
  }
"""

testSection :: String
testSection = """
  { "type": "TabSection"
  , "name": "Name"
  , "fields":
    [ """ <> firstName <> """
    , """ <> lastName <> """
    , """ <> active <> """
    ]
  }
"""

testSequence :: String
testSequence = """
  { "type": "TabSequence"
  , "name": "Users"
  , "key": "users"
  , "template": """ <> testTemplate <> """
  , "values": """ <> value (UserInput ("[" <> testSection <> "]")) <> """
  }
"""

testTemplate :: String
testTemplate = """
  { "name": "User"
  , "fields": [""" <> testTemplateFullName <> """]
  }
"""

testTemplateFullName :: String
testTemplateFullName =
  testField
    "Full Name"
    "Enter the full name"
    testTemplateText

testTemplateText :: String
testTemplateText = """
  { "type": "TemplateText"
  , "required": """ <> val true "Boolean" <> """
  , "placeholder": """ <> val "" "String" <> """
  , "default": null
  , "maxLength": null
  , "minLength": null
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
  , "errors": [ """ <> requiredError <> """ ]
  }
"""

toggleInput :: String -> String
toggleInput v = """
  { "type": "Toggle"
  , "value": """ <> v <> """
  , "default": """ <> val false "Boolean" <> """
  , "errors": []
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
  UserCleared -> """
      { "type": "UserCleared" }
    """
  NotSet -> """
      { "type": "NotSet" }
    """

requiredError :: String
requiredError = """
  { "type": "Required" }
"""
