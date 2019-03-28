module Lynx.Data.Form where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array as Data.Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Newtype (wrap)
import Data.Set (Set, toUnfoldable)
import Data.Set as Data.Set
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Lynx.Data.Expr (EvalError, Expr, ExprType, Key, array_, boolean_, cents_, evalExpr, if_, lookup_, pair_, print, string_, toArray, val_)
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
  , errors :: Errors ValidationError
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

type TypeaheadSingleRows f r =
  ( options :: f
  | r
  )

data Input f
  = Currency (Record (SharedRows f + RequiredRows f + CurrencyRows f + ()))
  | DateTime (Record (SharedRows f + RequiredRows f + DateTimeRows f + ()))
  | Dropdown (Record (SharedRows f + RequiredRows f + DropdownRows f + ()))
  | Text (Record (SharedRows f + RequiredRows f + StringRows f ()))
  | Toggle (Record (SharedRows f ()))
  | TypeaheadSingle (Record (SharedRows f + TypeaheadSingleRows f + ()))

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
    TypeaheadSingle r -> "type" := "TypeaheadSingle" ~> encodeJson r

instance decodeInput :: DecodeJson (Input Expr) where
  decodeJson json = do
    x <- decodeJson json
    x .: "type" >>= case _ of
      "Currency" -> pure <<< Currency <=< decodeJson $ json
      "DateTime" -> pure <<< DateTime <=< decodeJson $ json
      "Dropdown" -> pure <<< Dropdown <=< decodeJson $ json
      "Text" -> pure <<< Text <=< decodeJson $ json
      "Toggle" -> pure <<< Toggle <=< decodeJson $ json
      "TypeaheadSingle" -> pure <<< TypeaheadSingle <=< decodeJson $ json
      t -> Left $ "Unsupported Input type: " <> t

instance arbitraryInput :: Arbitrary (Input Expr) where
  arbitrary = genericArbitrary

data InputSource a
  = UserInput a
  | Invalid a
  | UserCleared
  | NotSet

derive instance eqInputSource :: (Eq a) => Eq (InputSource a)

derive instance functorInputSource :: Functor InputSource

derive instance genericInputSource :: Generic (InputSource a) _

instance foldableInputSource :: Foldable InputSource where
  foldMap f = case _ of
    UserInput x -> f x
    Invalid x -> f x
    UserCleared -> mempty
    NotSet -> mempty
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance traversableInputSource :: Traversable InputSource where
  sequence = sequenceDefault
  traverse f = case _ of
    UserInput x -> map UserInput (f x)
    Invalid x -> map Invalid (f x)
    UserCleared -> pure UserCleared
    NotSet -> pure NotSet

instance showInputSource :: (Show a) => Show (InputSource a) where
  show = genericShow

instance encodeInputSource :: (EncodeJson a) => EncodeJson (InputSource a) where
  encodeJson = case _ of
    UserInput x -> "type" := "UserInput" ~> "value" := x ~> jsonEmptyObject
    Invalid x -> "type" := "Invalid" ~> "value" := x ~> jsonEmptyObject
    UserCleared -> "type" := "UserCleared" ~> jsonEmptyObject
    NotSet -> "type" := "NotSet" ~> jsonEmptyObject

instance decodeInputSource :: (DecodeJson a) => DecodeJson (InputSource a) where
  decodeJson json = do
    x' <- decodeJson json
    x' .: "type" >>= case _ of
      "UserInput" -> x' .: "value" >>= (pure <<< UserInput)
      "Invalid" -> x' .: "value" >>= (pure <<< Invalid)
      "UserCleared" -> pure UserCleared
      "NotSet" -> pure NotSet
      x -> Left $ x <> " is not a valid InputSource"

instance arbitraryInputSource :: (Arbitrary a) => Arbitrary (InputSource a) where
  arbitrary = genericArbitrary

userInput :: forall a. InputSource a -> Maybe a
userInput = case _ of
  UserInput x -> Just x
  _ -> Nothing

newtype Errors e = Errors (Set e)

derive instance eqErrors :: Eq e => Eq (Errors e)

derive instance genericErrors :: Generic (Errors e) _

instance showErrors :: Show e => Show (Errors e) where
  show = genericShow

instance semigroupErrors :: Ord e => Semigroup (Errors e) where
  append (Errors a) (Errors b) = Errors (a <> b)

instance monoidErrors :: Ord e => Monoid (Errors e) where
  mempty = Errors mempty

instance arbitraryErrors :: (Arbitrary e, Ord e) => Arbitrary (Errors e) where
  arbitrary = pure mempty

instance encodeErrors :: (EncodeJson e, Ord e) => EncodeJson (Errors e) where
  encodeJson (Errors e) = encodeJson e

instance decodeErrors :: (DecodeJson e, Ord e) => DecodeJson (Errors e) where
  decodeJson = map Errors <<< decodeJson

singletonError :: ∀ e. Ord e => e -> Errors e
singletonError = Errors <<< Data.Set.singleton

noErrors :: ∀ e. Ord e => Errors e -> Boolean
noErrors (Errors e) = Data.Set.isEmpty e

errorsToArray :: ∀ e. Ord e => Errors e -> Array e
errorsToArray (Errors e) = toUnfoldable e

data ValidationError
  = Required
  | MinLength Int
  | MaxLength Int
  | InvalidOption String

derive instance eqValidationError :: Eq ValidationError

derive instance ordValidationError :: Ord ValidationError

derive instance genericValidationError :: Generic ValidationError _

instance showValidationError :: Show ValidationError where
  show = genericShow

instance encodeValidationError :: EncodeJson ValidationError where
  encodeJson = case _ of
    Required -> "type" := "Required" ~> jsonEmptyObject
    MinLength x -> "type" := "MinLength" ~> "param" := x ~> jsonEmptyObject
    MaxLength x -> "type" := "MaxLength" ~> "param" := x ~> jsonEmptyObject
    InvalidOption x -> "type" := "InvalidOption" ~> "param" := x ~> jsonEmptyObject

instance decodeValidationError :: DecodeJson ValidationError where
  decodeJson json = do
    x' <- decodeJson json
    x' .: "type" >>= case _ of
      "Required" -> pure Required
      "MinLength" -> x' .: "param" >>= (pure <<< MinLength)
      "MaxLength" -> x' .: "param" >>= (pure <<< MaxLength)
      "InvalidOption" -> x' .: "param" >>= (pure <<< InvalidOption)
      x -> Left $ x <> " is not a supported ValidationError"

instance arbitraryValidationError :: Arbitrary ValidationError where
  arbitrary = genericArbitrary

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
      pure $ validate $ Currency
        { default
        , placeholder
        , required
        , value
        , errors: mempty
        }
    DateTime input -> do
      default <- traverse (evalExpr get) input.default
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      value <- traverse (evalExpr get) input.value
      pure $ validate $ DateTime
        { default
        , placeholder
        , required
        , value
        , errors: mempty
        }
    Dropdown input -> do
      default <- traverse (evalExpr get) input.default
      options <- evalExpr get input.options
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      value' <- traverse (evalExpr get) input.value
      let arrayOptions = Data.Maybe.fromMaybe [] (toArray options)
          value = case value' of
            UserInput x ->
              if (not $ x `Data.Array.elem` arrayOptions)
                then Invalid x
                else UserInput x
            otherwise -> otherwise
      pure $ validate $ Dropdown
        { default
        , options
        , placeholder
        , required
        , value
        , errors: mempty
        }
    Text input -> do
      default <- traverse (evalExpr get) input.default
      maxLength <- traverse (evalExpr get) input.maxLength
      minLength <- traverse (evalExpr get) input.minLength
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      value <- traverse (evalExpr get) input.value
      pure $ validate $ Text
        { default
        , maxLength
        , minLength
        , placeholder
        , required
        , value
        , errors: mempty
        }
    Toggle input -> do
      default <- traverse (evalExpr get) input.default
      value <- traverse (evalExpr get) input.value
      pure $ validate $ Toggle { default, value, errors: mempty }
    TypeaheadSingle input -> do
      default <- traverse (evalExpr get) input.default
      options <- evalExpr get input.options
      value <- traverse (evalExpr get) input.value
      pure $ validate $ TypeaheadSingle
        { default
        , options
        , value
        , errors: mempty
        }

  validate :: Input ExprType -> Input ExprType
  validate = case _ of
    Currency input ->
      if displayError input
        then Currency $
          input { errors = input.errors <> validateRequired input }
        else Currency input
    DateTime input ->
      if displayError input
        then DateTime $
          input { errors = input.errors <> validateRequired input }
        else DateTime input
    Dropdown input ->
      if displayError input
        then Dropdown $
          input { errors = input.errors <> validateInvalid input <> validateRequired input }
        else Dropdown input
    Text input ->
      if displayError input
        then Text $
          input { errors = input.errors <> validateRequired input }
        else Text input
    Toggle input -> Toggle input
    TypeaheadSingle input -> TypeaheadSingle input

  validateRequired
    :: ∀ r
     . Record (SharedRows ExprType + ( required :: ExprType | r ))
    -> Errors ValidationError
  validateRequired input =
    if input.required == boolean_ true && isEmpty (getValue input)
      then singletonError Required
      else mempty

  validateInvalid
    :: ∀ r
     . Record (SharedRows ExprType + r )
    -> Errors ValidationError
  validateInvalid input =
    case input.value of
      Invalid x -> singletonError (InvalidOption $ print x)
      otherwise -> mempty

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
      TypeaheadSingle typeahead -> getValue typeahead

displayError
  :: ∀ r
   . Record (SharedRows ExprType r)
  -> Boolean
displayError x = x.value /= NotSet

isEmpty :: Maybe ExprType -> Boolean
isEmpty Nothing = true
isEmpty (Just x) = case x of
  Lynx.Data.Expr.Array []   -> true
  Lynx.Data.Expr.String ""  -> true
  Lynx.Data.Expr.Array _    -> false
  Lynx.Data.Expr.Boolean _  -> false
  Lynx.Data.Expr.Cents _    -> false
  Lynx.Data.Expr.DateTime _ -> false
  Lynx.Data.Expr.Int _      -> false
  Lynx.Data.Expr.Pair _     -> false
  Lynx.Data.Expr.String _   -> false

getValue
  :: ∀ a r
   . Record (SharedRows a r)
  -> Maybe a
getValue x = userInput x.value <|> x.default

setValue :: Key -> InputSource ExprType -> Page Expr -> Page Expr
setValue key val page = page { contents = map setSection page.contents }
  where
  setSection :: Section Expr -> Section Expr
  setSection section = section { contents = map setField section.contents }

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
      TypeaheadSingle input ->
        field { input = TypeaheadSingle input { value = map val_ val } }
    | otherwise = field

-- MVP

mvpPage :: Page Expr
mvpPage =
  { name: "New Campaign Request"
  , contents:
    [ { name: "Campaign"
      , contents:
        [ mvpName
        , mvpTargetableInterest
        , mvpFacebookTwitterPage
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
      , errors: mempty
      }
  , key: "end"
  , name: val_ (string_ "End")
  , visibility: val_ (boolean_ true)
  }

mvpFacebookTwitterPage :: Field Expr
mvpFacebookTwitterPage =
  { description: val_ (string_ "")
  , input:
    TypeaheadSingle
      { default: Nothing
      , options
      , value: NotSet
      , errors: mempty
      }
  , key: "facebook-twitter-page"
  , name: val_ (string_ "Facebook / Twitter Page")
  , visibility: val_ (boolean_ true)
  }
  where
  options :: Expr
  options =
    val_
    ( array_
      [ pair_ { name: string_ "ABC News", value: string_ "ABC News"}
      , pair_ { name: string_ "CBS New York", value: string_ "CBS New York"}
      , pair_ { name: string_ "NBC News", value: string_ "NBC News"}
      ]
    )

mvpMediaBudget :: Field Expr
mvpMediaBudget =
  { description: val_ (string_ "")
  , input:
    Currency
      { default: Nothing
      , errors: mempty
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
      , errors: mempty
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
      , errors: mempty
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
    ( array_
      [ pair_ { name: string_ "App Installs", value: string_ "App Installs" }
      , pair_ { name: string_ "Brand Awareness", value: string_ "Brand Awareness" }
      , pair_ { name: string_ "Conversions", value: string_ "Conversions" }
      , pair_ { name: string_ "Event Responses", value: string_ "Event Responses" }
      , pair_ { name: string_ "Lead Generation", value: string_ "Lead Generation" }
      , pair_ { name: string_ "Link Clicks", value: string_ "Link Clicks" }
      , pair_ { name: string_ "Offer Claims", value: string_ "Offer Claims" }
      , pair_ { name: string_ "Page Likes", value: string_ "Page Likes" }
      , pair_ { name: string_ "Post Engagement", value: string_ "Post Engagement" }
      , pair_ { name: string_ "Reach", value: string_ "Reach" }
      , pair_ { name: string_ "VideoViews", value: string_ "VideoViews" }
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
      , errors: mempty
      }
  , key: "start"
  , name: val_ (string_ "Start")
  , visibility: val_ (boolean_ true)
  }

mvpTargetableInterest :: Field Expr
mvpTargetableInterest =
  { description: val_ (string_ "")
  , input:
    TypeaheadSingle
      { default: Nothing
      , options
      , value: NotSet
      , errors: mempty
      }
  , key: "targetable-interest"
  , name: val_ (string_ "Targetable Interest")
  , visibility: val_ (boolean_ true)
  }
  where
  options :: Expr
  options =
    val_
    ( array_
      [ pair_ { name: string_ "ABC", value: string_ "ABC" }
      , pair_ { name: string_ "CBS", value: string_ "CBS" }
      , pair_ { name: string_ "NBC", value: string_ "NBC" }
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
    , errors: mempty
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
    , errors: mempty
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
    , errors: mempty
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
      if_
        do lookup_ "active" (val_ $ boolean_ false)
        do val_ $
          array_
            [ pair_ { name: string_ "Strawberry", value: string_ "Strawberry" }
            , pair_ { name: string_ "Blueberry", value: string_ "Blueberry" }
            ]
        do val_ $
          array_
            [ pair_ { name: string_ "Apple", value: string_ "Apple" }
            , pair_ { name: string_ "Banana", value: string_ "Banana" }
            , pair_ { name: string_ "Cherry", value: string_ "Cherry" }
            ]
    , placeholder: val_ (string_ "Choose a food")
    , required: val_ (boolean_ true)
    , value: NotSet
    , errors: mempty
    }
  }

money :: Field Expr
money =
  { name: val_ (string_ "Money")
  , visibility: val_ (boolean_ true)
  , description: val_ (string_ "")
  , key: "money"
  , input: Currency
    { default: Nothing
    , placeholder: val_ (cents_ (wrap zero))
    , required: val_ (boolean_ true)
    , value: NotSet
    , errors: mempty
    }
  }
