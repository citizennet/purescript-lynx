module Lynx.Form where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array as Data.Array
import Data.Either (Either(..), note)
import Data.Foldable (class Foldable, foldM, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty as Data.NonEmpty
import Data.Set (Set, toUnfoldable)
import Data.Set as Data.Set
import Data.Traversable (class Traversable, for, sequenceDefault, traverse)
import Effect.Aff (error, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Lynx.Expr (EvalError, Expr, ExprType, Key, array_, boolean_, cents_, evalExpr, if_, isEmpty, lookup_, pair_, print, string_, toArray, toString, val_)
import Network.HTTP.Affjax as Network.HTTP.Affjax
import Network.HTTP.Affjax.Response as Network.HTTP.Affjax.Response
import Network.RemoteData (RemoteData)
import Network.RemoteData as Network.RemoteData
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Type.Row (type (+))

type Page f =
  { name :: String
  , tabs :: NonEmpty Array (Tab f)
  }

type Tab f =
  { name :: String
  , link :: String
  , sections :: NonEmpty Array (TabSections f)
  }

data TabSections f
  = TabSection (Section f)
  | TabSequence (Sequence f)

type Section f =
  { name :: String
  , fields :: NonEmpty Array (Field f)
  }

type Sequence f =
  { name :: String
  , key :: String
  , template :: TemplateSection f
  , values :: InputSource (Array (Section f))
  }

stamp :: forall a. Key -> Page a -> Page a
stamp key page = page { tabs = map stampTab page.tabs }
  where
  stampTab :: Tab a -> Tab a
  stampTab tab = tab { sections = map stampTabSections tab.sections }

  stampTabSections :: TabSections a -> TabSections a
  stampTabSections tabSections' = case tabSections' of
    TabSection section -> TabSection section
    TabSequence sequence
      | sequence.key == key ->
        TabSequence
          sequence
            { values = stampInputSource sequence.template sequence.values
            }
      | otherwise -> TabSequence sequence

  stampInputSource :: TemplateSection a -> InputSource (Array (Section a)) -> InputSource (Array (Section a))
  stampInputSource templateSection inputSource = case inputSource of
    UserInput userInput' -> UserInput (userInput' <> [stamp'])
    Invalid invalid -> Invalid (invalid <> [stamp'])
    UserCleared -> UserInput [stamp']
    NotSet -> UserInput [stamp']
    where
    stamp' = templateSection { fields = map stampField templateSection.fields }

  stampField :: TemplateField a -> Field a
  stampField templateField = templateField { input = stampInput templateField.input }

  stampInput :: TemplateInput a -> Input a
  stampInput templateInput = case templateInput of
    TemplateCurrency currency ->
      Currency
        { default: currency.default
        , errors: mempty
        , placeholder: currency.placeholder
        , required: currency.required
        , value: NotSet
        }
    TemplateDateTime dateTime ->
      DateTime
        { default: dateTime.default
        , errors: mempty
        , placeholder: dateTime.placeholder
        , required: dateTime.required
        , value: NotSet
        }
    TemplateDropdown dropdown ->
      Dropdown
        { default: dropdown.default
        , errors: mempty
        , options: dropdown.options
        , placeholder: dropdown.placeholder
        , required: dropdown.required
        , value: NotSet
        }
    TemplateText text ->
      Text
        { default: text.default
        , errors: mempty
        , maxLength: text.maxLength
        , minLength: text.minLength
        , placeholder: text.placeholder
        , required: text.required
        , value: NotSet
        }
    TemplateToggle toggle ->
      Toggle
        { default: toggle.default
        , errors: mempty
        , value: NotSet
        }
    TemplateTypeaheadSingle typeaheadSingle ->
      TypeaheadSingle
        { default: typeaheadSingle.default
        , errors: mempty
        , options: typeaheadSingle.options
        , resultValue: typeaheadSingle.resultValue
        , results: typeaheadSingle.results
        , uri: typeaheadSingle.uri
        , value: NotSet
        }

unstamp :: forall a. Key -> Int -> Page a -> Page a
unstamp key index page = page { tabs = map unstampTab page.tabs }
  where
  unstampTab :: Tab a -> Tab a
  unstampTab tab = tab { sections = map unstampTabSections tab.sections }

  unstampTabSections :: TabSections a -> TabSections a
  unstampTabSections tabSections' = case tabSections' of
    TabSection section -> TabSection section
    TabSequence sequence
      | sequence.key == key ->
        TabSequence
          sequence
            { values = unstampInputSource sequence.template sequence.values
            }
      | otherwise -> TabSequence sequence

  unstampInputSource :: TemplateSection a -> InputSource (Array (Section a)) -> InputSource (Array (Section a))
  unstampInputSource templateSection inputSource = case inputSource of
    UserInput userInput' ->
      UserInput (Data.Maybe.fromMaybe userInput' $ Data.Array.deleteAt index userInput')
    Invalid invalid ->
      Invalid (Data.Maybe.fromMaybe invalid $ Data.Array.deleteAt index invalid)
    UserCleared ->
      UserCleared
    NotSet ->
      NotSet

tabSections
  :: forall a b
   . (Section a -> b)
  -> (Sequence a -> b)
  -> TabSections a
  -> b
tabSections f g = case _ of
  TabSection section -> f section
  TabSequence sequence -> g sequence

derive instance eqTabSections :: Eq f => Eq (TabSections f)

derive instance genericTabSections :: Generic (TabSections f) _

instance showTabSections :: Show (TabSections Expr) where
  show = genericShow

instance arbitraryTabSections :: Arbitrary (TabSections Expr) where
  arbitrary = genericArbitrary

instance encodeJsonTabSections :: EncodeJson (TabSections Expr) where
  encodeJson = case _ of
    TabSection section -> "type" := "TabSection" ~> encodeJson section
    TabSequence sequence -> "type" := "TabSequence" ~> encodeJson sequence

instance decodeJsonTabSections :: DecodeJson (TabSections Expr) where
  decodeJson json = do
    x <- decodeJson json
    x .: "type" >>= case _ of
      "TabSection" -> pure <<< TabSection <=< decodeJson $ json
      "TabSequence" -> pure <<< TabSequence <=< decodeJson $ json
      t -> Left $ "Unsupported TabSections type: " <> t

type TemplateSection f =
  { name :: String
  , fields :: NonEmpty Array (TemplateField f)
  }

type TemplateField f =
  { name :: f
  , visibility :: f
  , description :: f
  , key :: Key
  , input :: TemplateInput f
  }

data TemplateInput f
  = TemplateCurrency
    { default :: Maybe f
    , required :: f
    , placeholder :: f
    }
  | TemplateDateTime
    { default :: Maybe f
    , required :: f
    , placeholder :: f
    }
  | TemplateDropdown
    { default :: Maybe f
    , required :: f
    , options :: f
    , placeholder :: f
    }
  | TemplateText
    { default :: Maybe f
    , required :: f
    , placeholder :: f
    , maxLength :: Maybe f
    , minLength :: Maybe f
    }
  | TemplateToggle
    { default :: Maybe f
    }
  | TemplateTypeaheadSingle
    { default :: Maybe f
    , options :: f
    , resultValue :: f
    , results :: f
    , uri :: f
    }

derive instance eqTemplateInput :: Eq f => Eq (TemplateInput f)

derive instance genericTemplateInput :: Generic (TemplateInput f) _

instance showTemplateInput :: Show (TemplateInput Expr) where
  show = genericShow

instance arbitraryTemplateInput :: Arbitrary (TemplateInput Expr) where
  arbitrary = genericArbitrary

instance encodeJsonTemplateInput :: EncodeJson (TemplateInput Expr) where
  encodeJson = case _ of
    TemplateCurrency r -> "type" := "TemplateCurrency" ~> encodeJson r
    TemplateDateTime r -> "type" := "TemplateDateTime" ~> encodeJson r
    TemplateDropdown r -> "type" := "TemplateDropdown" ~> encodeJson r
    TemplateText r -> "type" := "TemplateText" ~> encodeJson r
    TemplateToggle r -> "type" := "TemplateToggle" ~> encodeJson r
    TemplateTypeaheadSingle r -> "type" := "TemplateTypeaheadSingle" ~> encodeJson r

instance decodeJsonTemplateInput :: DecodeJson (TemplateInput Expr) where
  decodeJson json = do
    x <- decodeJson json
    x .: "type" >>= case _ of
      "TemplateCurrency" -> pure <<< TemplateCurrency <=< decodeJson $ json
      "TemplateDateTime" -> pure <<< TemplateDateTime <=< decodeJson $ json
      "TemplateDropdown" -> pure <<< TemplateDropdown <=< decodeJson $ json
      "TemplateText" -> pure <<< TemplateText <=< decodeJson $ json
      "TemplateToggle" -> pure <<< TemplateToggle <=< decodeJson $ json
      "TemplateTypeaheadSingle" -> pure <<< TemplateTypeaheadSingle <=< decodeJson $ json
      t -> Left $ "Unsupported TemplateInput type: " <> t

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

-- Gabe finds these labels confusing. He'd like a clear definition
type TypeaheadSingleRows f r =
  ( options :: f
  , resultValue :: f
  , results :: f
  , uri :: f
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

errors :: forall a. Field a -> Errors ValidationError
errors { input } = case input of
  Currency currency -> currency.errors
  DateTime dateTime -> dateTime.errors
  Dropdown dropdown -> dropdown.errors
  Text text -> text.errors
  Toggle toggle -> toggle.errors
  TypeaheadSingle typeaheadSingle -> typeaheadSingle.errors

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
  tabs <- traverse evalTab page.tabs
  pure page { tabs = tabs }
  where
  evalTab :: Tab Expr -> Either EvalError (Tab ExprType)
  evalTab tab =
    tab { sections = _ } <$> traverse evalTabSections tab.sections

  evalTabSections :: TabSections Expr -> Either EvalError (TabSections ExprType)
  evalTabSections =
    tabSections
      (map TabSection <$> evalSection)
      (map TabSequence <$> evalSequence)

  evalSection :: Section Expr -> Either EvalError (Section ExprType)
  evalSection section =
    section { fields = _ } <$> traverse evalField section.fields

  evalSequence :: Sequence Expr -> Either EvalError (Sequence ExprType)
  evalSequence sequence = ado
    values <- (traverse <<< traverse) evalSection sequence.values
    template <- evalTemplate sequence.template
    in sequence { values = values, template = template }

  evalTemplate :: TemplateSection Expr -> Either EvalError (TemplateSection ExprType)
  evalTemplate template =
    template { fields = _ } <$> traverse evalTemplateField template.fields

  evalTemplateField :: TemplateField Expr -> Either EvalError (TemplateField ExprType)
  evalTemplateField templateField = do
    description <- evalExpr get templateField.description
    input <- evalTemplateInput templateField.input
    name <- evalExpr get templateField.name
    visibility <- evalExpr get templateField.visibility
    pure { description, key: templateField.key, input, name, visibility }

  evalTemplateInput :: TemplateInput Expr -> Either EvalError (TemplateInput ExprType)
  evalTemplateInput = case _ of
    TemplateCurrency input -> ado
      default <- traverse (evalExpr get) input.default
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      in TemplateCurrency
        { default
        , placeholder
        , required
        }
    TemplateDateTime input -> ado
      default <- traverse (evalExpr get) input.default
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      in TemplateDateTime
        { default
        , placeholder
        , required
        }
    TemplateDropdown input -> ado
      default <- traverse (evalExpr get) input.default
      options <- evalExpr get input.options
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      in TemplateDropdown
        { default
        , options
        , placeholder
        , required
        }
    TemplateText input -> ado
      default <- traverse (evalExpr get) input.default
      maxLength <- traverse (evalExpr get) input.maxLength
      minLength <- traverse (evalExpr get) input.minLength
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      in TemplateText
        { default
        , maxLength
        , minLength
        , placeholder
        , required
        }
    TemplateToggle input ->
      TemplateToggle <<< { default: _ } <$> traverse (evalExpr get) input.default

    TemplateTypeaheadSingle input -> ado
      default <- traverse (evalExpr get) input.default
      options <- evalExpr get input.options
      resultValue <- evalExpr get input.resultValue
      results <- evalExpr get input.results
      uri <- evalExpr get input.uri
      in TemplateTypeaheadSingle
        { default
        , options
        , resultValue
        , results
        , uri
        }

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
      resultValue <- evalExpr get input.resultValue
      results <- evalExpr get input.results
      uri <- evalExpr get input.uri
      value <- traverse (evalExpr get) input.value
      pure $ validate $ TypeaheadSingle
        { default
        , options
        , resultValue
        , results
        , uri
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
keys page = foldMap keysTab page.tabs
  where
  keysTab :: Tab Expr -> Map Key ExprType
  keysTab tab = foldMap (tabSections keysSection keysSequence) tab.sections

  keysSection :: Section Expr -> Map Key ExprType
  keysSection section = foldMap keysField section.fields

  keysSequence :: Sequence Expr -> Map Key ExprType
  keysSequence sequence = foldMap (foldMap keysSection) sequence.values

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

getValue
  :: ∀ a r
   . Record (SharedRows a r)
  -> Maybe a
getValue x = userInput x.value <|> x.default

setValue :: Key -> InputSource ExprType -> Page Expr -> Page Expr
setValue key val page = page { tabs = map setTab page.tabs }
  where
  setTab :: Tab Expr -> Tab Expr
  setTab tab = tab { sections = setTabSections <$> tab.sections }

  setTabSections :: TabSections Expr -> TabSections Expr
  setTabSections =
    tabSections
      (TabSection <<< setSection)
      (TabSequence <<< setSequence)

  setSection :: Section Expr -> Section Expr
  setSection section = section { fields = setField <$> section.fields }

  setSequence :: Sequence Expr -> Sequence Expr
  setSequence sequence = case sequence.values of
    UserInput sections -> sequence { values = UserInput $ setSection <$> sections }
    Invalid sections -> sequence { values = Invalid $ setSection <$> sections }
    _ -> sequence

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

parseTypeaheadJSON
  :: forall r
  . { results :: ExprType, resultValue :: ExprType | r }
  -> Json
  -> Either String (Array ExprType)
parseTypeaheadJSON { results, resultValue } json' = do
  resultsFields' <- parseArray "`results`" results
  resultsFields <- traverse (parseString "`results`") resultsFields'
  valueFields' <- parseArray "`resultValue`" resultValue
  valueFields <- traverse (parseString "`resultValue`") valueFields'
  resultsJSON' <- foldM decodeField json' resultsFields
  resultsJSON <- decodeJson resultsJSON'
  for resultsJSON \result -> do
    value' <- foldM decodeField result valueFields
    value <- decodeJson value'
    pure (pair_ { name: string_ value, value: string_ value })
  where
  decodeField :: Json -> String -> Either String Json
  decodeField json field = do
    obj <- decodeJson json
    obj .: field

  parseArray :: String -> ExprType -> Either String (Array ExprType)
  parseArray field = note (field <> " not an Array") <<< toArray

  parseString :: String -> ExprType -> Either String String
  parseString field = note (field <> " not an Array of Strings") <<< toString

asyncFromTypeahead
  :: forall f r
  . MonadAff f
  => { resultValue :: ExprType, results :: ExprType, uri :: ExprType | r }
  -> String
  -> f (RemoteData String (Array ExprType))
asyncFromTypeahead typeahead x = case toString typeahead.uri of
  Just uri -> do
    { response } <-
      liftAff
        (Network.HTTP.Affjax.get Network.HTTP.Affjax.Response.json $ uri <> x)
    pure (Network.RemoteData.fromEither $ parseTypeaheadJSON typeahead response)
  Nothing ->
    liftAff (throwError $ error $ print typeahead.uri <> " is not a String")

-- MVP

mvpPage :: Page Expr
mvpPage =
  { name: "New Campaign Request"
  , tabs:
    Data.NonEmpty.NonEmpty
      { name: "Details"
      , link: "details"
      , sections: Data.NonEmpty.singleton mvpDetailsSection
      }
      [ { name: "Creative"
        , link: "creative"
        , sections: Data.NonEmpty.singleton mvpCreativeSequence
        }
      ]
  }

mvpDetailsSection :: TabSections Expr
mvpDetailsSection =
  TabSection
    { name: "Campaign"
    , fields:
      Data.NonEmpty.NonEmpty
      mvpName
      [ mvpTargetableInterest
      , mvpFacebookTwitterPage
      , mvpObjective
      , mvpMediaBudget
      , mvpStart
      , mvpEnd
      ]
    }


mvpCreativeSequence :: TabSections Expr
mvpCreativeSequence =
  TabSequence
    { name: "Creative"
    , key: "creative"
    , template:
      { name: "Social Creative"
      , fields:
        Data.NonEmpty.singleton
          { description: val_ (string_ "Copy to display with the creative")
          , key: "copy"
          , input:
            TemplateText
            { default: Nothing
            , required: val_ (boolean_ true)
            , placeholder: val_ (string_ "I don't know what a creative is")
            , maxLength: Nothing
            , minLength: Nothing
            }
          , name: val_ (string_ "Copy")
          , visibility: val_ (boolean_ true)
          }
      }
    , values:
      UserInput
      [ { name: "Social Creative"
        , fields: Data.NonEmpty.singleton mvpSocialAccount
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
      , options: val_ (array_ [])
      , resultValue: val_ (array_ [string_ "name"])
      , results: val_ (array_ [string_ "facebookTwitterPage"])
      , uri: val_ (string_ mvpURI)
      , value: NotSet
      , errors: mempty
      }
  , key: "facebook-twitter-page"
  , name: val_ (string_ "Facebook / Twitter Page")
  , visibility: val_ (boolean_ true)
  }

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

mvpSocialAccount :: Field Expr
mvpSocialAccount =
  { description: val_ (string_ "")
  , input: Toggle
    { default: Nothing
    , value: NotSet
    , errors: mempty
    }
  , key: "social-account"
  , name: val_ (string_ "Social Account")
  , visibility: val_ (boolean_ true)
  }

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
      , options: val_ (array_ [])
      , resultValue: val_ (array_ [string_ "name"])
      , results: val_ (array_ [string_ "targetableInterest"])
      , uri: val_ (string_ mvpURI)
      , value: NotSet
      , errors: mempty
      }
  , key: "targetable-interest"
  , name: val_ (string_ "Targetable Interest")
  , visibility: val_ (boolean_ true)
  }

mvpURI :: String
mvpURI =
  "https://raw.githubusercontent.com/citizennet/purescript-lynx/706ef89d4e2f4ebdf5a6fc485936fd4d3973b5e8/src/mvp.json?ignore="

-- Test

testPage :: Page Expr
testPage =
  { name: "Profile"
  , tabs:
    Data.NonEmpty.singleton
      { name: "User"
      , link: "user"
      , sections : Data.NonEmpty.singleton $ TabSection testSection
      }
  }

testSection :: Section Expr
testSection =
  { name: "Name"
  , fields:
    Data.NonEmpty.NonEmpty
      firstName
      [ lastName
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
