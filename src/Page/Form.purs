module Lynx.Page.Form where

import Prelude

import Data.Bitraversable (bitraverse_)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Data.Expr (EvalError(..), Expr, ExprType(..), Key, reflectType)
import Lynx.Data.Form (Field, Input(..), Page, Section, testPage)
import Lynx.Data.Form as Lynx.Data.Form
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
    H.modify_ _ { values = Lynx.Data.Form.keys testPage }
    void $ bitraverse_
      (\e -> H.modify _ { form = Failure e })
      (\f -> eval $ EvalForm f a)
      (Right testPage)

  eval (EvalForm expr a) = a <$ do
    { values } <- H.get
    let evaled = Lynx.Data.Form.eval (\key -> Data.Map.lookup key values) expr
    H.modify_ _ { form = map { expr, evaled: _ } (fromEither evaled) }

  eval (UpdateKey key val a) = do
    H.modify_ \state -> state { values = Data.Map.insert key val state.values }
    { form } <- H.get
    case form of
      Success { expr } -> eval (EvalForm (Lynx.Data.Form.set key val expr) a)
      _ -> pure a

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
