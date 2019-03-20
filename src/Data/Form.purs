module Lynx.Data.Form where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Coproduct.Inject (inj)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Lynx.Data.Expr (EvalError, Expr, boolean_, cents_, evalExpr, string_)
import Lynx.Data.ExprType (ExprType, StringF(..), array_, pair_)
import Lynx.Data.If (if_)
import Lynx.Data.Lookup (Key, lookup_)
import Lynx.Data.Val (val_)
import Matryoshka (embed)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Type.Row (type (+))

type LayoutRows c r =
  ( name :: String
  , contents :: Array c
  | r
  )

type Page f = Record (LayoutRows (Section f) ())

type Section f = Record (LayoutRows (Field f) ())

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
  , value :: InputSource f
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

type CurrencyRows f r =
  ( placeholder :: f
  | r
  )

type DropdownRows f r =
  ( options :: f
  , placeholder :: f
  | r
  )

data Input f
  = Currency (Record (SharedRows f + RequiredRows f + CurrencyRows f + ()))
  | Dropdown (Record (SharedRows f + RequiredRows f + DropdownRows f + ()))
  | Text (Record (SharedRows f + RequiredRows f + StringRows f ()))
  | Toggle (Record (SharedRows f ()))

derive instance eqInput :: (Eq f) => Eq (Input f)

derive instance genericInput :: Generic (Input f) _
instance showInput :: Show (Input Expr) where show = genericShow

instance encodeInput :: EncodeJson (Input Expr) where
  encodeJson = case _ of
    Currency r -> "type" := "Currency" ~> encodeJson r
    Dropdown r -> "type" := "Dropdown" ~> encodeJson r
    Text r -> "type" := "Text" ~> encodeJson r
    Toggle r -> "type" := "Toggle" ~> encodeJson r

instance decodeInput :: DecodeJson (Input Expr) where
  decodeJson json = do
    x <- decodeJson json
    x .: "type" >>= case _ of
      "Currency" -> pure <<< Currency <=< decodeJson $ json
      "Dropdown" -> pure <<< Dropdown <=< decodeJson $ json
      "Text" -> pure <<< Text <=< decodeJson $ json
      "Toggle" -> pure <<< Toggle <=< decodeJson $ json
      t -> Left $ "Unsupported Input type: " <> t

instance arbitraryInput :: Arbitrary (Input Expr) where
  arbitrary = genericArbitrary

data InputSource a
  = UserInput a
  | Invalid a
  | NotSet

derive instance eqInputSource :: (Eq a) => Eq (InputSource a)

derive instance functorInputSource :: Functor InputSource

derive instance genericInputSource :: Generic (InputSource a) _

instance foldableInputSource :: Foldable InputSource where
  foldMap f = case _ of
    UserInput x -> f x
    Invalid x -> f x
    NotSet -> mempty
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance traversableInputSource :: Traversable InputSource where
  sequence = sequenceDefault
  traverse f = case _ of
    UserInput x -> map UserInput (f x)
    Invalid x -> map Invalid (f x)
    NotSet -> pure NotSet

instance showInputSource :: (Show a) => Show (InputSource a) where
  show = genericShow

instance encodeInputSource :: (EncodeJson a) => EncodeJson (InputSource a) where
  encodeJson = case _ of
    UserInput x -> "type" := "UserInput" ~> "value" := x ~> jsonEmptyObject
    Invalid x -> "type" := "Invalid" ~> "value" := x ~> jsonEmptyObject
    NotSet -> "type" := "NotSet" ~> jsonEmptyObject

instance decodeInputSource :: (DecodeJson a) => DecodeJson (InputSource a) where
  decodeJson json = do
    x' <- decodeJson json
    x' .: "type" >>= case _ of
      "UserInput" -> x' .: "value" >>= (pure <<< UserInput)
      "Invalid" -> x' .: "value" >>= (pure <<< Invalid)
      "NotSet" -> pure NotSet
      x -> Left $ x <> " is not a valid InputSource"

instance arbitraryInputSource :: (Arbitrary a) => Arbitrary (InputSource a) where
  arbitrary = genericArbitrary

userInput :: forall a. InputSource a -> Maybe a
userInput = case _ of
  UserInput x -> Just x
  _ -> Nothing

eval :: (Key -> Maybe ExprType) -> Page Expr -> Either EvalError (Page ExprType)
eval get page = do
  contents <- traverse evalSection page.contents
  pure page { contents = contents }
  where
  evalSection :: Section Expr -> Either EvalError (Section ExprType)
  evalSection section = do
    contents <- traverse evalField section.contents
    pure section { contents = contents }

  evalField :: Field Expr -> Either EvalError (Field ExprType)
  evalField field = do
    description <- evalExpr get field.description
    input <- evalInput field.input
    name <- evalExpr get field.name
    visibility <- evalExpr get field.visibility
    pure { description, key: field.key, input, name, visibility }

  evalInput :: Input Expr -> Either EvalError (Input ExprType)
  evalInput = case _ of
    Currency input -> do
      default <- traverse (evalExpr get) input.default
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      value <- traverse (evalExpr get) input.value
      pure
        ( Currency
          { default
          , placeholder
          , required
          , value
          }
        )
    Dropdown input -> do
      default <- traverse (evalExpr get) input.default
      options <- evalExpr get input.options
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      value <- traverse (evalExpr get) input.value
      pure
        ( Dropdown
          { default
          , options
          , placeholder
          , required
          , value
          }
        )
    Text input -> do
      default <- traverse (evalExpr get) input.default
      maxLength <- traverse (evalExpr get) input.maxLength
      minLength <- traverse (evalExpr get) input.minLength
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      value <- traverse (evalExpr get) input.value
      pure
        ( Text
          { default
          , maxLength
          , minLength
          , placeholder
          , required
          , value
          }
        )
    Toggle input -> do
      default <- traverse (evalExpr get) input.default
      value <- traverse (evalExpr get) input.value
      pure (Toggle { default, value })

keys :: Page Expr -> Map Key ExprType
keys page = foldMap keysSection page.contents
  where
  keysSection :: Section Expr -> Map Key ExprType
  keysSection section = foldMap keysField section.contents

  keysField :: Field Expr -> Map Key ExprType
  keysField field = case value of
    Just expr -> case evalExpr (const Nothing) expr of
      Left _ -> mempty
      Right x -> Data.Map.singleton field.key x
    Nothing -> mempty
    where
    value = case field.input of
      Currency currency -> getValue currency
      Dropdown dropdown -> getValue dropdown
      Text text -> getValue text
      Toggle toggle -> getValue toggle

getValue
  :: ∀ a r
   . Record (SharedRows a r)
  -> Maybe a
getValue x = userInput x.value <|> x.default

setValue :: Key -> ExprType -> Page Expr -> Page Expr
setValue key val page = page { contents = map setSection page.contents}
  where
  setSection :: Section Expr -> Section Expr
  setSection section = section { contents = map setField section.contents}

  setField :: Field Expr -> Field Expr
  setField field
    | key == field.key = case field.input of
      Currency input ->
        field { input = Currency input { value = UserInput (val_ val) } }
      Dropdown input ->
        field { input = Dropdown input { value = UserInput (val_ val) } }
      Text input ->
        field { input = Text input { value = UserInput (val_ val) } }
      Toggle input ->
        field { input = Toggle input { value = UserInput (val_ val) } }
    | otherwise = field

-- MVP

mvpPage :: Page Expr
mvpPage =
  { name: "New Campaign Request"
  , contents:
    [ { name: "Campaign"
      , contents:
        [ mvpName
        , mvpObjective
        , mvpMediaBudget
        ]
      }
    ]
  }

mvpMediaBudget :: Field Expr
mvpMediaBudget =
  { description: string_ ""
  , input:
    Currency
      { default: Nothing
      , placeholder: cents_ (wrap zero)
      , required: boolean_ true
      , value: NotSet
      }
  , key: "media-budget"
  , name: string_ "Media Budget"
  , visibility: boolean_ true
  }

mvpName :: Field Expr
mvpName =
  { description: string_ ""
  , input:
    Text
      { default: Nothing
      , maxLength: Nothing
      , minLength: Nothing
      , placeholder: string_ ""
      , required: boolean_ true
      , value: NotSet
      }
  , key: "name"
  , name: string_ "Name"
  , visibility: boolean_ true
  }

mvpObjective :: Field Expr
mvpObjective =
  { description: string_ ""
  , input:
    Dropdown
      { default: Nothing
      , options
      , placeholder: string_ "Choose an objective"
      , required: boolean_ true
      , value: NotSet
      }
  , key: "objective"
  , name: string_ "Objective"
  , visibility: boolean_ true
  }
  where
  options :: Expr
  options =
    val_
    ( array_
      [ pair_ { name: embed (inj $ String "App Installs"), value: embed (inj $ String "App Installs") }
      , pair_ { name: embed (inj $ String "Brand Awareness"), value: embed (inj $ String "Brand Awareness") }
      , pair_ { name: embed (inj $ String "Conversions"), value: embed (inj $ String "Conversions") }
      , pair_ { name: embed (inj $ String "Event Responses"), value: embed (inj $ String "Event Responses") }
      , pair_ { name: embed (inj $ String "Lead Generation"), value: embed (inj $ String "Lead Generation") }
      , pair_ { name: embed (inj $ String "Link Clicks"), value: embed (inj $ String "Link Clicks") }
      , pair_ { name: embed (inj $ String "Offer Claims"), value: embed (inj $ String "Offer Claims") }
      , pair_ { name: embed (inj $ String "Page Likes"), value: embed (inj $ String "Page Likes") }
      , pair_ { name: embed (inj $ String "Post Engagement"), value: embed (inj $ String "Post Engagement") }
      , pair_ { name: embed (inj $ String "Reach"), value: embed (inj $ String "Reach") }
      , pair_ { name: embed (inj $ String "VideoViews"), value: embed (inj $ String "VideoViews") }
      ]
    )

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
    , food
    , money
    ]
  }

firstName :: Field Expr
firstName =
  { name: string_ "First Name"
  , visibility: boolean_ true
  , description: string_ "Enter your first name"
  , key: "firstName"
  , input: Text
    { default: Just (string_ "John")
    , maxLength: Nothing
    , minLength: Nothing
    , placeholder: string_ ""
    , required: boolean_ true
    , value: NotSet
    }
  }

lastName :: Field Expr
lastName =
  { name: string_ "Last Name"
  , visibility: boolean_ true
  , description: string_ "Enter your last name"
  , key: "lastName"
  , input: Text
    { default: Just (string_ "Smith")
    , maxLength: Nothing
    , minLength: Nothing
    , placeholder: string_ ""
    , required: boolean_ true
    , value: NotSet
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
    , value: NotSet
    }
  }
  where
  description = if_ (lookup_ "active" $ boolean_ true)
    (string_ "User's account is active!")
    (string_ "User's account is not active")

food :: Field Expr
food =
  { name: string_ "Favorite Food"
  , visibility: boolean_ true
  , description: string_ "What is your favorite food?"
  , key: "food"
  , input: Dropdown
    { default: Nothing
    , options:
      if_
        do lookup_ "active" (boolean_ false)
        do val_ $
          array_
            [ pair_ { name: embed (inj $ String "Strawberry"), value: embed (inj $ String "Strawberry") }
            , pair_ { name: embed (inj $ String "Blueberry"), value: embed (inj $ String "Blueberry") }
            ]
        do val_ $
          array_
            [ pair_ { name: embed (inj $ String "Apple"), value: embed (inj $ String "Apple") }
            , pair_ { name: embed (inj $ String "Banana"), value: embed (inj $ String "Banana") }
            , pair_ { name: embed (inj $ String "Cherry"), value: embed (inj $ String "Cherry") }
            ]
    , placeholder: string_ ""
    , required: boolean_ true
    , value: NotSet
    }
  }

money :: Field Expr
money =
  { name: string_ "money"
  , visibility: boolean_ true
  , description: string_ ""
  , key: "money"
  , input: Currency
    { default: Nothing
    , placeholder: cents_ (wrap zero)
    , required: boolean_ true
    , value: NotSet
    }
  }
