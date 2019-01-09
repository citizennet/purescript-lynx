module Formal.Page.Form where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Formal.Data.Behavior (Form(..), Input(..), Page(..), Section(..), testPageEither)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Block.Toggle (toggle) as Toggle
import Ocelot.HTML.Properties (css)

type State =
  { form :: Either String Page
  }

data Query a = Noop a

type ParentInput = String

type Message = Void

component
  :: ∀ m
   . H.Component HH.HTML Query ParentInput Message m
component =
  H.component
    { initialState
    , eval
    , render
    , receiver: const Nothing
    }
  where

  initialState :: ParentInput -> State
  initialState s =
    { form: testPageEither
    }

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (Noop a) = pure a

  render :: State -> H.ComponentHTML Query
  render { form } =
    Layout.section_
      case form of
        Left e ->
          [ Card.card_ [ Format.p [ css "text-red" ] [ HH.text e ] ] ]
        Right (Page page) ->
          append
          [ Format.heading_
            [ HH.text page.name ]
          ]
          $ renderSection <$> page.contents

  renderSection :: Section -> H.ComponentHTML Query
  renderSection (Section section) =
    Card.card_ $
      append
      [ Format.subHeading_ [ HH.text section.name ] ]
      $ renderForm <$> section.contents

  renderForm :: Form -> H.ComponentHTML Query
  renderForm (Form form) =
    FormField.field_
      { label: HH.text $ show form.name
      , helpText: Just $ show form.description
      , error: Nothing
      , inputId: form.key
      }
      [ renderInput $ Form form ]

  renderInput :: Form -> H.ComponentHTML Query
  renderInput (Form form) = case form.input of
    Text input ->
      Input.input
        [ HP.placeholder $ show input.placeholder
        , HP.id_ $ show form.key
        ]
    Toggle toggle ->
      Toggle.toggle
        [ HP.checked true
        , HP.id_ $ show form.key
        ]
