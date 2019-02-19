module Lynx.Page.Form where

import Prelude

import Control.Comonad (extract)
import Data.Bitraversable (bitraverse_)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Heterogeneous.Mapping (hmap)
import Lynx.Data.Expr (Expr)
import Lynx.Data.Form (Field(..), Input(..), Page(..), Section(..), testPageEither)
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Block.Toggle (toggle) as Toggle
import Ocelot.HTML.Properties (css)

type State =
  { form :: Either String ({ raw :: Page Expr, cur :: Page Identity })
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
      testPageEither

  eval (EvalForm form a) = a <$ do
    -- let cur = hmap (EvalExpr { lookups: Object.empty }) form
    -- H.modify_ _ { form = pure { raw: form, cur } }
    pure unit

  render :: State -> H.ComponentHTML Query
  render { form } =
    Layout.section_
      case form of
        Left e ->
          [ Card.card_ [ Format.p [ css "text-red" ] [ HH.text e ] ] ]
        Right ({ cur: page }) ->
          append
          [ Format.heading_
            [ HH.text page.name ]
          ]
          $ renderSection <$> page.contents

  renderSection :: Section Identity -> H.ComponentHTML Query
  renderSection (section) =
    Card.card_ $
      append
      [ Format.subHeading_ [ HH.text section.name ] ]
      $ renderField <$> section.contents

  renderField :: Field Identity -> H.ComponentHTML Query
  renderField (field) =
    FormField.field_
      { label: HH.text $ extract field.name
      , helpText: Just $ extract field.description
      , error: Nothing
      , inputId: field.key
      }
      [ renderInput field ]

  renderInput :: Field Identity -> H.ComponentHTML Query
  renderInput (field) = case field.input of
    Text input ->
      Input.input
        [ HP.placeholder $ extract input.placeholder
        , HP.id_ field.key
        ]
    Toggle toggle ->
      Toggle.toggle
        [ HP.checked true
        , HP.id_ field.key
        ]
