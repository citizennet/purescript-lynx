module Lynx.Page.Form where

import Prelude

import Data.Bitraversable (bitraverse_)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
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

type State = forall o.
  { form :: Either String { expr :: Page Expr, evaled :: Page Identity }
  , values :: Map Key o
  }

data Query a
  = Initialize a
  | EvalForm (Page Expr) a

type ParentInput = String

type Message = Void

component
  :: ∀ m o
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
    void $ bitraverse_
      (\e -> H.modify _ { form = Left e })
      (\f -> eval $ EvalForm f a)
      (Right testPage)

  eval (EvalForm expr a) = a <$ do
    values <- H.gets _.values
    let evaled = evalPage get expr
        get :: Key -> Maybe o
        get key = Data.Map.lookup key values
    H.modify_ _ { form = pure { expr, evaled } }
    pure unit

  evalPage :: (Key -> Maybe o) -> Page Expr -> Page Identity
  evalPage get page = page
    { contents = map (evalSection get) page.contents
    }

  evalSection :: (Key -> Maybe o) -> Section Expr -> Section Identity
  evalSection get section = section
    { contents = map (evalField get) section.contents
    }

  evalField :: (Key -> Maybe o) -> Field Expr -> Field Identity
  evalField get field = field
    { description = pure (evalExpr' get field.description)
    , input = evalInput field.input
    , name = pure (evalExpr' get field.name)
    , visibility = pure (evalExpr' get field.visibility)
    }

  evalInput :: Input Expr -> Input Identity
  evalInput = case _ of
    Text input -> Text ?input
    Toggle input -> Toggle ?input

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
