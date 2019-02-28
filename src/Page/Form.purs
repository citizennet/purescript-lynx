module Lynx.Page.Form where

import Prelude

import Data.Bitraversable (bitraverse_)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Data.Expr (EvalError(..), Expr(..), ExprType(..), Key, evalExpr, reflectType)
import Lynx.Data.Form (Field, Input(..), InputSource(..), InputState, Page, Section, testPage)
import Network.RemoteData (RemoteData(..), fromEither)
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Block.Toggle (toggle) as Toggle
import Ocelot.HTML.Properties (css)

type State =
  { form :: RemoteData EvalError { expr :: Page Expr, evaled :: Page ExprType }
  , values :: Map Key ExprType
  }

data Query a
  = Initialize a
  | EvalForm (Page Expr) a
  | UpdateKey Key ExprType a

type ParentInput = String

type Message = Void

component
  :: ∀ m
   . H.Component HH.HTML Query ParentInput Message m
component =
  H.lifecycleComponent
    { initialState: const initialState
    , eval
    , render
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState =
    { form: Loading
    , values: mempty
    }

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (Initialize a) = a <$ do
    H.modify_ _ { values = keysPage testPage }
    void $ bitraverse_
      (\e -> H.modify _ { form = Failure e })
      (\f -> eval $ EvalForm f a)
      (Right testPage)

  eval (EvalForm expr a) = a <$ do
    { values } <- H.get
    let evaled = evalPage (\key -> Data.Map.lookup key values) expr
    H.modify_ _ { form = map { expr, evaled: _ } (fromEither evaled) }

  eval (UpdateKey key val a) = do
    H.modify_ \state -> state { values = Data.Map.insert key val state.values }
    { form } <- H.get
    case form of
      Success { expr } -> eval (EvalForm (setPage key val expr) a)
      _ -> pure a

  evalPage :: (Key -> Maybe ExprType) -> Page Expr -> Either EvalError (Page ExprType)
  evalPage get page = do
    contents <- traverse (evalSection get) page.contents
    pure page { contents = contents }

  evalSection :: (Key -> Maybe ExprType) -> Section Expr -> Either EvalError (Section ExprType)
  evalSection get section = do
    contents <- traverse (evalField get) section.contents
    pure section { contents = contents }

  evalField :: (Key -> Maybe ExprType) -> Field Expr -> Either EvalError (Field ExprType)
  evalField get field = do
    description <- evalExpr get field.description
    input <- evalInput get field.input
    name <- evalExpr get field.name
    visibility <- evalExpr get field.visibility
    pure { description, key: field.key, input, name, visibility }

  evalInput :: (Key -> Maybe ExprType) -> Input Expr -> Either EvalError (Input ExprType)
  evalInput get = case _ of
    Text input -> do
      default <- traverse (evalExpr get) input.default
      maxLength <- traverse (evalExpr get) input.maxLength
      minLength <- traverse (evalExpr get) input.minLength
      placeholder <- evalExpr get input.placeholder
      required <- evalExpr get input.required
      value <- evalInputState get input.value
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
      value <- evalInputState get input.value
      pure (Toggle { default, value })

  evalInputState :: (Key -> Maybe ExprType) -> Record (InputState Expr ()) -> Either EvalError (Record (InputState ExprType ()))
  evalInputState get inputState = do
    value <- traverse (evalExpr get) inputState.value
    pure inputState { value = value }

  keysPage :: Page Expr -> Map Key ExprType
  keysPage page = foldMap keysSection page.contents

  keysSection :: Section Expr -> Map Key ExprType
  keysSection section = foldMap keysField section.contents

  keysField :: Field Expr -> Map Key ExprType
  keysField field = case field.input of
    Text { value: { value: Just expr } }
      | Right value <- evalExpr (const Nothing) expr ->
        Data.Map.singleton field.key value
    Text { default: Just expr }
      | Right value <- evalExpr (const Nothing) expr ->
        Data.Map.singleton field.key value
    Toggle { value: { value: Just expr } }
      | Right value <- evalExpr (const Nothing) expr ->
        Data.Map.singleton field.key value
    Toggle { default: Just expr }
      | Right value <- evalExpr (const Nothing) expr ->
        Data.Map.singleton field.key value
    _ -> mempty

  render :: State -> H.ComponentHTML Query
  render { form } =
    Layout.section_
      case form of
        NotAsked ->
          []
        Loading ->
          [ Card.card_ [ Format.p [ css "text-red" ] [ HH.text "Loading..." ] ] ]
        Failure e ->
          [ Card.card_ [ Format.p [ css "text-red" ] [ renderEvalError e ] ] ]
        Success page ->
          append
          [ Format.heading_
            [ HH.text page.evaled.name ]
          ]
          $ renderSection <$> page.evaled.contents

  renderEvalError :: forall a. EvalError -> H.ComponentHTML a
  renderEvalError = case _ of
    IfCondition x ->
      HH.text $
        "Expected conditional to be a Boolean, but its type is: "
          <> reflectType x
    EqualMismatch x ->
      HH.text $
        "Expected both sides of equal to have the same type,"
          <> " but they are different."
          <> " left: "
          <> reflectType x.left
          <> " right: "
          <> reflectType x.right

  renderSection :: Section ExprType -> H.ComponentHTML Query
  renderSection (section) =
    Card.card_ $
      append
      [ Format.subHeading_ [ HH.text section.name ] ]
      $ renderField <$> section.contents

  renderField :: Field ExprType -> H.ComponentHTML Query
  renderField (field) =
    FormField.field_
      { label: HH.text $ show field.name
      , helpText: Just $ show field.description
      , error: Nothing
      , inputId: field.key
      }
      [ renderInput field ]

  renderInput :: Field ExprType -> H.ComponentHTML Query
  renderInput (field) = case field.input of
    Text input ->
      Input.input
        [ HP.placeholder $ show input.placeholder
        , HP.id_ field.key
        ]
    Toggle toggle ->
      Toggle.toggle
        [ HP.checked true
        , HP.id_ field.key
        , HE.onChecked (HE.input $ UpdateKey field.key <<< Boolean)
        ]

  setPage :: Key -> ExprType -> Page Expr -> Page Expr
  setPage key val page =
    page { contents = map (setSection key val) page.contents}

  setSection :: Key -> ExprType -> Section Expr -> Section Expr
  setSection key val section =
    section { contents = map (setField key val) section.contents}

  setField :: Key -> ExprType -> Field Expr -> Field Expr
  setField key val field
    | key == field.key = case field.input of
      Text input ->
        let value = { source: Just UserInput,  value: Just (Val val) }
        in field { input = Text input { value = value }}
      Toggle input ->
        let value = { source: Just UserInput,  value: Just (Val val) }
        in field { input = Toggle input { value = value }}
    | otherwise = field
