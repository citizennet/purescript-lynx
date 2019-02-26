module Lynx.Page.Form where

import Prelude

import Data.Bitraversable (bitraverse_)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lynx.Data.Expr (Expr, Key, evalExpr')
import Lynx.Data.Form (Field, Input(..), Page, Section, testPage)
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Block.Toggle (toggle) as Toggle
import Ocelot.HTML.Properties (css)

type State =
  { form :: Either String { expr :: Page Expr, evaled :: Page Identity }
  , values :: Map Key Boolean
  }

data Query a
  = Initialize a
  | EvalForm (Page Expr) a

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
    { form: Left "Loading..."
    , values: mempty
    }

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (Initialize a) = a <$ do
    values <- H.gets _.values
    let get key = Data.Map.lookup key values
    void $ bitraverse_
      (\e -> H.modify _ { form = Left e })
      (\f -> eval $ EvalForm f a)
      (Right $ testPage get)

  eval (EvalForm expr a) = a <$ do
    values <- H.gets _.values
    let evaled = evalPage expr
        get key = Data.Map.lookup key values
    H.modify_ _ { form = pure { expr, evaled } }
    pure unit

  evalPage :: Page Expr -> Page Identity
  evalPage page = page
    { contents = map evalSection page.contents
    }

  evalSection :: Section Expr -> Section Identity
  evalSection section = section
    { contents = map evalField section.contents
    }

  evalField :: Field Expr -> Field Identity
  evalField field = field
    { description = pure (evalExpr' field.description)
    , input = evalInput field.input
    , name = pure (evalExpr' field.name)
    , visibility = pure (evalExpr' field.visibility)
    }

  evalInput :: Input Expr -> Input Identity
  evalInput = case _ of
    Text input -> Text
      { default: map (pure <<< evalExpr') input.default
      , maxLength: map (pure <<< evalExpr') input.maxLength
      , minLength: map (pure <<< evalExpr') input.minLength
      , placeholder: (pure <<< evalExpr') input.placeholder
      , required: (pure <<< evalExpr') input.required
      , value: input.value
      }
    Toggle input -> Toggle
      { default: map (pure <<< evalExpr') input.default
      , value: input.value
      }

  render :: State -> H.ComponentHTML Query
  render { form } =
    Layout.section_
      case form of
        Left e ->
          [ Card.card_ [ Format.p [ css "text-red" ] [ HH.text e ] ] ]
        Right page ->
          append
          [ Format.heading_
            [ HH.text page.evaled.name ]
          ]
          $ renderSection <$> page.evaled.contents

  renderSection :: Section Identity -> H.ComponentHTML Query
  renderSection (section) =
    Card.card_ $
      append
      [ Format.subHeading_ [ HH.text section.name ] ]
      $ renderField <$> section.contents

  renderField :: Field Identity -> H.ComponentHTML Query
  renderField (field) =
    FormField.field_
      { label: HH.text $ show field.name
      , helpText: Just $ show field.description
      , error: Nothing
      , inputId: field.key
      }
      [ renderInput field ]

  renderInput :: Field Identity -> H.ComponentHTML Query
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
        ]
