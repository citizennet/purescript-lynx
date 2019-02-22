module Lynx.Data.Form where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromString, jsonParser, (.:), (:=), (~>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Lynx.Data.Expr (Expr)
import Type.Row (type (+))

-- newtype EvalExpr = EvalExpr
  -- { lookups :: Object (Array String)
  -- }

-- instance evalMaybeExpr
  -- :: Expressible o
  -- => Mapping EvalExpr (Maybe (Expr o)) (Maybe (Identity o))
  -- where
  -- mapping f = map $ mapping f
-- else instance evalArr
  -- :: HMap EvalExpr (n Expr) (n Identity)
  -- => Mapping EvalExpr (Array (n Expr)) (Array (n Identity))
  -- where
  -- mapping f = map $ hmap f
-- else instance evalExpr
  -- :: Expressible o
  -- => Mapping EvalExpr (Expr o) (Identity o)
  -- where
  -- mapping (EvalExpr f) = do
    -- pure <<< evalExpr'
-- else instance evalInput
  -- :: Mapping EvalExpr (Input Expr) (Input Identity)
  -- where
  -- mapping f = hmap f
-- else instance evalVal
  -- :: Mapping EvalExpr a a
  -- where
  -- mapping f = identity

type LayoutRows c r =
  ( name :: String
  , contents :: Array c
  | r
  )

-- newtype Page f = Page (Record (LayoutRows (Section f) ()))

type Page f = Record (LayoutRows (Section f) ())

-- derive instance newtypePage :: Newtype (Page f) _
-- derive instance genericPage :: Generic (Page f) _
-- instance showPage :: Show (Page Expr) where show = genericShow

-- instance encodePage :: EncodeJson (Page Expr) where
  -- encodeJson = encodeJson <<< unwrap

-- instance decodePage :: DecodeJson (Page Expr) where
  -- decodeJson = pure <<< wrap <=< decodeJson

-- instance hmapPage
  -- :: HMap EvalExpr (Page Expr) (Page Identity)
  -- where
  -- hmap f = over Page $ hmap f

-- newtype Section f = Section (Record (LayoutRows (Field f) ()))

type Section f = Record (LayoutRows (Field f) ())

-- derive instance newtypeSection :: Newtype (Section f) _
-- derive instance genericSection :: Generic (Section f) _
-- instance showSection :: Show (Section Expr) where show = genericShow

-- instance encodeSection :: EncodeJson (Section Expr) where
  -- encodeJson = encodeJson <<< unwrap

-- instance decodeSection :: DecodeJson (Section Expr) where
  -- decodeJson = pure <<< wrap <=< decodeJson

-- instance hmapSection
  -- :: HMap EvalExpr (Section Expr) (Section Identity)
  -- where
  -- hmap f = over Section $ hmap f

type Key = String

type FieldRows f r =
  ( name :: f String
  , visibility :: f Boolean
  , description :: f String
  , key :: Key
  , input :: Input f
  | r
  )

-- newtype Field f = Field (Record (FieldRows f ()))

type Field f = Record (FieldRows f ())

-- derive instance newtypeField :: Newtype (Field f) _
-- derive instance genericField :: Generic (Field f) _
-- instance showField :: Show (Field Expr) where show = genericShow

-- instance encodeField :: EncodeJson (Field Expr) where
  -- encodeJson = encodeJson <<< unwrap

-- instance decodeField :: DecodeJson (Field Expr) where
  -- decodeJson = pure <<< wrap <=< decodeJson

-- instance hmapField
  -- :: HMap EvalExpr (Field Expr) (Field Identity)
  -- where
  -- hmap f = over Field $ hmap f

type SharedRows f t r =
  ( default :: Maybe (f t)
  , value :: Record (InputState t ())
  | r
  )

type RequiredRows f r =
  ( required :: f Boolean
  | r
  )

type StringRows f r =
  ( placeholder :: f String
  , maxLength :: Maybe (f Int)
  , minLength :: Maybe (f Int)
  | r
  )

type InputState t r =
  ( value :: Maybe t
  , source :: Maybe InputSource
  | r
  )

data Input f
  = Text (Record (SharedRows f String + RequiredRows f + StringRows f ()))
  | Toggle (Record (SharedRows f Boolean ()))

derive instance genericInput :: Generic (Input f) _
instance showInput :: Show (Input Expr) where show = genericShow
-- instance hmapInput
  -- :: HMap EvalExpr (Input Expr) (Input Identity)
  -- where
  -- hmap f (Text r) = Text $ hmap f r
  -- hmap f (Toggle r) = Toggle $ hmap f r

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
