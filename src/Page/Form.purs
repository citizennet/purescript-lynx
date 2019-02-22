module Lynx.Page.Form where

import Prelude

import Data.Bitraversable (bitraverse_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lynx.Data.Expr (Expr)
import Lynx.Data.Form (Field, Input(..), Page, Section, testPage)
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Block.Toggle (toggle) as Toggle
import Ocelot.HTML.Properties (css)

type State =
  { form :: Either String (Page Expr)
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
    }

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (Initialize a) = a <$ do
    void $ bitraverse_
      (\e -> H.modify _ { form = Left e })
      (\f -> eval $ EvalForm f a)
      (Right testPage)

  eval (EvalForm form a) = a <$ do
    H.modify_ _ { form = pure form }
    pure unit

  render :: State -> H.ComponentHTML Query
  render { form } =
    Layout.section_
      case form of
        Left e ->
          [ Card.card_ [ Format.p [ css "text-red" ] [ HH.text e ] ] ]
        Right page ->
          append
          [ Format.heading_
            [ HH.text page.name ]
          ]
          $ renderSection <$> page.contents

  renderSection :: Section Expr -> H.ComponentHTML Query
  renderSection (section) =
    Card.card_ $
      append
      [ Format.subHeading_ [ HH.text section.name ] ]
      $ renderField <$> section.contents

  renderField :: Field Expr -> H.ComponentHTML Query
  renderField (field) =
    FormField.field_
      { label: HH.text $ show field.name
      , helpText: Just $ show field.description
      , error: Nothing
      , inputId: field.key
      }
      [ renderInput field ]

  renderInput :: Field Expr -> H.ComponentHTML Query
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
