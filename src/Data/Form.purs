module Lynx.Data.Form where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Lynx.Data.Expr (EvalError, Expr(..), ExprType, Key, boolean_, cents_, evalExpr, if_, lookup_, string_, val_)
import Lynx.Data.Expr as Lynx.Data.Expr
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

type DateTimeRows f r =
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
  | DateTime (Record (SharedRows f + RequiredRows f + DateTimeRows f + ()))
  | Dropdown (Record (SharedRows f + RequiredRows f + DropdownRows f + ()))
  | Text (Record (SharedRows f + RequiredRows f + StringRows f ()))
  | Toggle (Record (SharedRows f ()))

derive instance eqInput :: (Eq f) => Eq (Input f)

derive instance genericInput :: Generic (Input f) _
instance showInput :: Show (Input Expr) where show = genericShow

instance encodeInput :: EncodeJson (Input Expr) where
  encodeJson = case _ of
    Currency r -> "type" := "Currency" ~> encodeJson r
    DateTime r -> "type" := "DateTime" ~> encodeJson r
    Dropdown r -> "type" := "Dropdown" ~> encodeJson r
    Text r -> "type" := "Text" ~> encodeJson r
    Toggle r -> "type" := "Toggle" ~> encodeJson r

instance decodeInput :: DecodeJson (Input Expr) where
  decodeJson json = do
    x <- decodeJson json
    x .: "type" >>= case _ of
      "Currency" -> pure <<< Currency <=< decodeJson $ json
      "DateTime" -> pure <<< DateTime <=< decodeJson $ json
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
  | UserCleared

derive instance eqInputSource :: (Eq a) => Eq (InputSource a)

derive instance functorInputSource :: Functor InputSource

derive instance genericInputSource :: Generic (InputSource a) _

instance foldableInputSource :: Foldable InputSource where
  foldMap f = case _ of
    UserInput x -> f x
    Invalid x -> f x
    NotSet -> mempty
    UserCleared -> mempty
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance traversableInputSource :: Traversable InputSource where
  sequence = sequenceDefault
  traverse f = case _ of
    UserInput x -> map UserInput (f x)
    Invalid x -> map Invalid (f x)
    NotSet -> pure NotSet
    UserCleared -> pure UserCleared

instance showInputSource :: (Show a) => Show (InputSource a) where
  show = genericShow

instance encodeInputSource :: (EncodeJson a) => EncodeJson (InputSource a) where
  encodeJson = case _ of
    UserInput x -> "type" := "UserInput" ~> "value" := x ~> jsonEmptyObject
    Invalid x -> "type" := "Invalid" ~> "value" := x ~> jsonEmptyObject
    NotSet -> "type" := "NotSet" ~> jsonEmptyObject
    UserCleared -> "type" := "UserCleared" ~> jsonEmptyObject

instance decodeInputSource :: (DecodeJson a) => DecodeJson (InputSource a) where
  decodeJson json = do
    x' <- decodeJson json
    x' .: "type" >>= case _ of
      "UserInput" -> x' .: "value" >>= (pure <<< UserInput)
      "Invalid" -> x' .: "value" >>= (pure <<< Invalid)
      "NotSet" -> pure NotSet
      "UserCleared" -> pure UserCleared
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
    DateTime input -> do
      default <- traverse (evalExpr get) input.default
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      value <- traverse (evalExpr get) input.value
      pure
        ( DateTime
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
      DateTime dateTime -> getValue dateTime
      Dropdown dropdown -> getValue dropdown
      Text text -> getValue text
      Toggle toggle -> getValue toggle

getValue
  :: ∀ a r
   . Record (SharedRows a r)
  -> Maybe a
getValue x = userInput x.value <|> x.default

setValue :: Key -> InputSource ExprType -> Page Expr -> Page Expr
setValue key val page = page { contents = map setSection page.contents}
  where
  setSection :: Section Expr -> Section Expr
  setSection section = section { contents = map setField section.contents}

  setField :: Field Expr -> Field Expr
  setField field
    | key == field.key = case field.input of
      Currency input ->
        field { input = Currency input { value = map val_ val } }
      DateTime input ->
        field { input = DateTime input { value = map val_ val } }
      Dropdown input ->
        field { input = Dropdown input { value = map val_ val } }
      Text input ->
        field { input = Text input { value = map val_ val } }
      Toggle input ->
        field { input = Toggle input { value = map val_ val } }
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
        , mvpStart
        , mvpEnd
        ]
      }
    ]
  }

mvpEnd :: Field Expr
mvpEnd =
  { description: val_ (string_ "")
  , input:
    DateTime
      { default: Nothing
      , placeholder: val_ (string_ "Choose a end date for the campaign")
      , required: val_ (boolean_ true)
      , value: NotSet
      }
  , key: "end"
  , name: val_ (string_ "End")
  , visibility: val_ (boolean_ true)
  }

mvpMediaBudget :: Field Expr
mvpMediaBudget =
  { description: val_ (string_ "")
  , input:
    Currency
      { default: Nothing
      , placeholder: val_ (cents_ (wrap zero))
      , required: val_ (boolean_ true)
      , value: NotSet
      }
  , key: "media-budget"
  , name: val_ (string_ "Media Budget")
  , visibility: val_ (boolean_ true)
  }

mvpName :: Field Expr
mvpName =
  { description: val_ (string_ "")
  , input:
    Text
      { default: Nothing
      , maxLength: Nothing
      , minLength: Nothing
      , placeholder: val_ (string_ "")
      , required: val_ (boolean_ true)
      , value: NotSet
      }
  , key: "name"
  , name: val_ (string_ "Name")
  , visibility: val_ (boolean_ true)
  }

mvpObjective :: Field Expr
mvpObjective =
  { description: val_ (string_ "")
  , input:
    Dropdown
      { default: Nothing
      , options
      , placeholder: val_ (string_ "Choose an objective")
      , required: val_ (boolean_ true)
      , value: NotSet
      }
  , key: "objective"
  , name: val_ (string_ "Objective")
  , visibility: val_ (boolean_ true)
  }
  where
  options :: Expr
  options =
    val_
    ( Lynx.Data.Expr.Array
      [ Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "App Installs"
        , value: Lynx.Data.Expr.String "App Installs"
        }
      , Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "Brand Awareness"
        , value: Lynx.Data.Expr.String "Brand Awareness"
        }
      , Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "Conversions"
        , value: Lynx.Data.Expr.String "Conversions"
        }
      , Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "Event Responses"
        , value: Lynx.Data.Expr.String "Event Responses"
        }
      , Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "Lead Generation"
        , value: Lynx.Data.Expr.String "Lead Generation"
        }
      , Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "Link Clicks"
        , value: Lynx.Data.Expr.String "Link Clicks"
        }
      , Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "Offer Claims"
        , value: Lynx.Data.Expr.String "Offer Claims"
        }
      , Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "Page Likes"
        , value: Lynx.Data.Expr.String "Page Likes"
        }
      , Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "Post Engagement"
        , value: Lynx.Data.Expr.String "Post Engagement"
        }
      , Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "Reach"
        , value: Lynx.Data.Expr.String "Reach"
        }
      , Lynx.Data.Expr.Pair
        { name: Lynx.Data.Expr.String "VideoViews"
        , value: Lynx.Data.Expr.String "VideoViews"
        }
      ]
    )

mvpStart :: Field Expr
mvpStart =
  { description: val_ (string_ "")
  , input:
    DateTime
      { default: Nothing
      , placeholder: val_ (string_ "Choose a start date for the campaign")
      , required: val_ (boolean_ true)
      , value: NotSet
      }
  , key: "start"
  , name: val_ (string_ "Start")
  , visibility: val_ (boolean_ true)
  }

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
  { name: val_ (string_ "First Name")
  , visibility: val_ (boolean_ true)
  , description: val_ (string_ "Enter your first name")
  , key: "firstName"
  , input: Text
    { default: Just (val_ (string_ "John"))
    , maxLength: Nothing
    , minLength: Nothing
    , placeholder: val_ (string_ "")
    , required: val_ (boolean_ true)
    , value: NotSet
    }
  }

lastName :: Field Expr
lastName =
  { name: val_ (string_ "Last Name")
  , visibility: val_ (boolean_ true)
  , description: val_ (string_ "Enter your last name")
  , key: "lastName"
  , input: Text
    { default: Just (val_ (string_ "Smith"))
    , maxLength: Nothing
    , minLength: Nothing
    , placeholder: val_ (string_ "")
    , required: val_ (boolean_ true)
    , value: NotSet
    }
  }

active :: Field Expr
active =
  { name: val_ (string_ "Active")
  , visibility: val_ (boolean_ true)
  , description
  , key: "active"
  , input: Toggle
    { default: Just (val_ (boolean_ true))
    , value: NotSet
    }
  }
  where
  description = if_ (lookup_ "active" $ val_ (boolean_ true))
    (val_ (string_ "User's account is active!"))
    (val_ (string_ "User's account is not active"))

food :: Field Expr
food =
  { name: val_ (string_ "Favorite Food")
  , visibility: val_ (boolean_ true)
  , description: val_ (string_ "What is your favorite food?")
  , key: "food"
  , input: Dropdown
    { default: Nothing
    , options:
      If
        do Lookup "active" (val_ $ Lynx.Data.Expr.Boolean false)
        do val_ $
          Lynx.Data.Expr.Array
            [ Lynx.Data.Expr.Pair
              { name: Lynx.Data.Expr.String "Strawberry"
              , value: Lynx.Data.Expr.String "Strawberry"
              }
            , Lynx.Data.Expr.Pair
              { name: Lynx.Data.Expr.String "Blueberry"
              , value: Lynx.Data.Expr.String "Blueberry"
              }
            ]
        do val_ $
          Lynx.Data.Expr.Array
            [ Lynx.Data.Expr.Pair
              { name: Lynx.Data.Expr.String "Apple"
              , value: Lynx.Data.Expr.String "Apple"
              }
            , Lynx.Data.Expr.Pair
              { name: Lynx.Data.Expr.String "Banana"
              , value: Lynx.Data.Expr.String "Banana"
              }
            , Lynx.Data.Expr.Pair
              { name: Lynx.Data.Expr.String "Cherry"
              , value: Lynx.Data.Expr.String "Cherry"
              }
            ]
    , placeholder: val_ (string_ "")
    , required: val_ (boolean_ true)
    , value: NotSet
    }
  }

money :: Field Expr
money =
  { name: val_ (string_ "money")
  , visibility: val_ (boolean_ true)
  , description: val_ (string_ "")
  , key: "money"
  , input: Currency
    { default: Nothing
    , placeholder: val_ (cents_ (wrap zero))
    , required: val_ (boolean_ true)
    , value: NotSet
    }
  }
