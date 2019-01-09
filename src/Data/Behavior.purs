module Formal.Data.Behavior where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, (.:), (:=), (~>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Formal.Data.Expr (Expr)
import Type.Row (type (+))

type LayoutRows c r =
  ( name :: String
  , contents :: Array c
  | r
  )

newtype Page = Page (Record (LayoutRows Section ()))

derive instance newtypePage :: Newtype Page _
derive instance genericPage :: Generic Page _
instance showPage :: Show Page where show = genericShow

newtype Section = Section (Record (LayoutRows Form ()))

derive instance newtypeSection :: Newtype Section _
derive instance genericSection :: Generic Section _
instance showSection :: Show Section where show = genericShow

newtype Form = Form
  { name :: Expr String
  , visibility :: Expr Boolean
  , description :: Expr String
  , key :: String
  , input :: Input
  }

derive instance newtypeForm :: Newtype Form _
derive instance genericForm :: Generic Form _
instance showForm :: Show Form where show = genericShow

type SharedRows t r =
  ( default :: Maybe (Expr t)
  | r
  )

type RequiredRows r =
  ( required :: Expr Boolean
  | r
  )

type StringRows r =
  ( placeholder :: Expr String
  , maxLength :: Maybe (Expr Int)
  , minLength :: Maybe (Expr Int)
  | r
  )

data Input
  = Text (Record (SharedRows String + RequiredRows + StringRows ()))
  | Toggle (Record (SharedRows Boolean ()))

derive instance genericInput :: Generic Input _
instance showInput :: Show Input where show = genericShow

instance encodePage :: EncodeJson Page where
  encodeJson = encodeJson <<< unwrap

instance encodeSection :: EncodeJson Section where
  encodeJson = encodeJson <<< unwrap

instance encodeForm :: EncodeJson Form where
  encodeJson = encodeJson <<< unwrap

instance encodeInput :: EncodeJson Input where
  encodeJson = case _ of
    Text r -> "type" := "Text" ~> encodeJson r
    Toggle r -> "type" := "Toggle" ~> encodeJson r

instance decodePage :: DecodeJson Page where
  decodeJson = pure <<< wrap <=< decodeJson

instance decodeSection :: DecodeJson Section where
  decodeJson = pure <<< wrap <=< decodeJson

instance decodeForm :: DecodeJson Form where
  decodeJson = pure <<< wrap <=< decodeJson

instance decodeInput :: DecodeJson Input where
  decodeJson json = do
    x <- decodeJson json
    x .: "type" >>= case _ of
      "Text" -> pure <<< Text <=< decodeJson $ json
      "Toggle" -> pure <<< Toggle <=< decodeJson $ json
      t -> Left $ "Unsupported Input type: " <> t

-- Test

testPageEither :: Either String Page
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

testForm :: String -> String -> String -> String
testForm n d i = """
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
  , "default": null
  , "maxLength": null
  , "minLength": null
  }
"""

toggleInput :: String
toggleInput = """
  { "type": "Toggle"
  , "default": """ <> val false "Boolean" <> """
  }
"""

firstName :: String
firstName = testForm "First Name" "Enter your first name" textInput

lastName :: String
lastName = testForm "Last Name" "Enter your last name" textInput

active :: String
active = testForm "Active" "Is user's account active" toggleInput

val :: ∀ a. Show a => a -> String -> String
val x o  = """
  { "op": "Val", "param": """ <> show x <> """, "in": "Void", "out": """ <> show o <> """ }
"""
