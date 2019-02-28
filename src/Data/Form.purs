module Lynx.Data.Form where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromString, (.:), (:=), (~>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Lynx.Data.Expr (Expr, boolean_, if_, lookup_, string_)
import Type.Row (type (+))

type LayoutRows c r =
  ( name :: String
  , contents :: Array c
  | r
  )

type Page f = Record (LayoutRows (Section f) ())

type Section f = Record (LayoutRows (Field f) ())

type Key = String

type FieldRows f r =
  ( name :: f
  , visibility :: f
  , description :: f
  , key :: Key
  , input :: Input f
  | r
  )

type Field f = Record (FieldRows f ())

type SharedRows f r =
  ( default :: Maybe f
  , value :: Record (InputState f ())
  | r
  )

type RequiredRows f r =
  ( required :: f
  | r
  )

type StringRows f r =
  ( placeholder :: f
  , maxLength :: Maybe f
  , minLength :: Maybe f
  | r
  )

type InputState t r =
  ( value :: Maybe t
  , source :: Maybe InputSource
  | r
  )

data Input f
  = Text (Record (SharedRows f + RequiredRows f + StringRows f ()))
  | Toggle (Record (SharedRows f ()))

derive instance genericInput :: Generic (Input f) _
instance showInput :: Show (Input Expr) where show = genericShow

instance encodeInput :: EncodeJson (Input Expr) where
  encodeJson = case _ of
    Text r -> "type" := "Text" ~> encodeJson r
    Toggle r -> "type" := "Toggle" ~> encodeJson r

instance decodeInput :: DecodeJson (Input Expr) where
  decodeJson json = do
    x <- decodeJson json
    x .: "type" >>= case _ of
      "Text" -> pure <<< Text <=< decodeJson $ json
      "Toggle" -> pure <<< Toggle <=< decodeJson $ json
      t -> Left $ "Unsupported Input type: " <> t

data InputSource
  = UserInput
  | DefaultValue
  | Invalid

derive instance genericInputSource :: Generic InputSource _
instance showInputSource :: Show InputSource where show = genericShow

instance encodeInputSource :: EncodeJson InputSource where
  encodeJson = fromString <<< case _ of
    UserInput -> "UserInput"
    DefaultValue -> "DefaultValue"
    Invalid -> "Invalid"

instance decodeInputSource :: DecodeJson InputSource where
  decodeJson = decodeJson >=> case _ of
    "UserInput" -> pure UserInput
    "DefaultValue" -> pure DefaultValue
    "Invalid" -> pure Invalid
    x -> Left $ x <> " is not a valid InputSource"

-- Test

testPage :: Page Expr
testPage =
  { name: "Profile"
  , contents:
    [ testSection
    ]
  }

testSection :: Section Expr
testSection =
  { name: "Name"
  , contents:
    [ firstName
    , lastName
    , active
    ]
  }

firstName :: Field Expr
firstName =
  { name: string_ "First Name"
  , visibility: boolean_ true
  , description: string_ "Enter your first name"
  , key: "firstName"
  , input: Text
    { default: Nothing
    , maxLength: Nothing
    , minLength: Nothing
    , placeholder: string_ ""
    , required: boolean_ true
    , value:
      { source: Nothing
      , value: Nothing
      }
    }
  }

lastName :: Field Expr
lastName =
  { name: string_ "Last Name"
  , visibility: boolean_ true
  , description: string_ "Enter your last name"
  , key: "lastName"
  , input: Text
    { default: Nothing
    , maxLength: Nothing
    , minLength: Nothing
    , placeholder: string_ ""
    , required: boolean_ true
    , value:
      { source: Nothing
      , value: Nothing
      }
    }
  }

active :: Field Expr
active =
  { name: string_ "Active"
  , visibility: boolean_ true
  , description
  , key: "active"
  , input: Toggle
    { default: Just (boolean_ true)
    , value:
      { source: Nothing
      , value: Nothing
      }
    }
  }
  where
  description = if_ (lookup_ "active")
    (string_ "User's account is active!")
    (string_ "User's account is not active")
