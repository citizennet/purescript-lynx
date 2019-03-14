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
import Data.Set (Set, toUnfoldable)
import Data.Set as Data.Set
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Lynx.Data.Expr (EvalError, Expr(..), ExprType(..), Key, boolean_, evalExpr, if_, lookup_, string_)
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

type DropdownRows f r =
  ( options :: f
  , placeholder :: f
  | r
  )

data Input f
  = Dropdown (Record (SharedRows f + RequiredRows f + DropdownRows f + ()))
  | Text (Record (SharedRows f + RequiredRows f + StringRows f ()))
  | Toggle (Record (SharedRows f ()))

derive instance eqInput :: (Eq f) => Eq (Input f)

derive instance genericInput :: Generic (Input f) _
instance showInput :: Show (Input Expr) where show = genericShow

instance encodeInput :: EncodeJson (Input Expr) where
  encodeJson = case _ of
    Dropdown r -> "type" := "Dropdown" ~> encodeJson r
    Text r -> "type" := "Text" ~> encodeJson r
    Toggle r -> "type" := "Toggle" ~> encodeJson r

instance decodeInput :: DecodeJson (Input Expr) where
  decodeJson json = do
    x <- decodeJson json
    x .: "type" >>= case _ of
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

instance decodeValidationError :: DecodeJson ValidationError where
  decodeJson json = do
    x' <- decodeJson json
    x' .: "type" >>= case _ of
      "Required" -> pure Required
      "MinLength" -> x' .: "param" >>= (pure <<< MinLength)
      "MaxLength" -> x' .: "param" >>= (pure <<< MaxLength)
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
    Dropdown input -> do
      default <- traverse (evalExpr get) input.default
      options <- evalExpr get input.options
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      value <- traverse (evalExpr get) input.value
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

  validate :: Input ExprType -> Input ExprType
  validate = case _ of
    Dropdown input ->
      if displayError input
        then Dropdown $ input { errors = input.errors <> validateRequired input }
        else Dropdown input
    Text input ->
      if displayError input
        then Text $ input { errors = input.errors <> validateRequired input }
        else Text input
    Toggle input -> Toggle input

  validateRequired
    :: ∀ r
     . Record (SharedRows ExprType + ( required :: ExprType | r ))
    -> Errors ValidationError
  validateRequired input = do
    if input.required == Boolean true && isEmpty (getValue input)
      then singletonError Required
      else mempty

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
      Dropdown dropdown -> getValue dropdown
      Text text -> getValue text
      Toggle toggle -> getValue toggle

displayError
  :: ∀ r
   . Record (SharedRows ExprType r)
  -> Boolean
displayError x = x.value /= NotSet

isEmpty :: Maybe ExprType -> Boolean
isEmpty Nothing = true
isEmpty (Just x) = case x of
  Array []  -> true
  String "" -> true
  Array _   -> false
  Boolean _ -> false
  Int _     -> false
  Pair _    -> false
  String _  -> false

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
      Dropdown input ->
        field { input = Dropdown input { value = UserInput (Val val) } }
      Text input ->
        field { input = Text input { value = UserInput (Val val) } }
      Toggle input ->
        field { input = Toggle input { value = UserInput (Val val) } }
    | otherwise = field

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
    , errors: mempty
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
    , errors: mempty
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
    , errors: mempty
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
      If
        do Lookup "active" (Val $ Boolean false)
        do Val $
          Array
            [ Pair { name: String "Strawberry", value: String "Strawberry" }
            , Pair { name: String "Blueberry", value: String "Blueberry" }
            ]
        do Val $
          Array
            [ Pair { name: String "Apple", value: String "Apple" }
            , Pair { name: String "Banana", value: String "Banana" }
            , Pair { name: String "Cherry", value: String "Cherry" }
            ]
    , placeholder: string_ ""
    , required: boolean_ true
    , value: NotSet
    , errors: mempty
    }
  }
