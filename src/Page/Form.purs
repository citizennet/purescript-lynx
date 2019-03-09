module Lynx.Page.Form where

import Prelude

import Data.Bitraversable (bitraverse_)
import Data.Either (Either(..))
import Data.Either.Nested (Either1)
import Data.Foldable (fold, foldMap, for_)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Data.Expr (EvalError(..), Expr, ExprType(..), Key, print, reflectType, toArray, toBoolean, toPair, toString)
import Lynx.Data.Form (Field, Input(..), Page, Section, getValue, testPage)
import Lynx.Data.Form as Lynx.Data.Form
import Network.RemoteData (RemoteData(..), fromEither)
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Block.Toggle (toggle) as Toggle
import Ocelot.Component.Dropdown as Dropdown
import Ocelot.Component.Dropdown.Render as Dropdown.Render
import Ocelot.HTML.Properties (css)

type State =
  { form :: RemoteData EvalError { expr :: Page Expr, evaled :: Page ExprType }
  , values :: Map Key ExprType
  }

data Query a
  = Initialize a
  | EvalForm (Page Expr) a
  | UpdateKey Key ExprType a
  | DropdownQuery Key (Dropdown.Message Query ExprType) a

type ParentInput = String

type ChildQuery m = Coproduct1 (DropdownQuery m)

type ChildSlot = Either1 Key

type DropdownQuery m = Dropdown.Query Query ExprType m

type Message = Void

component
  :: ∀ m
   .  MonadAff m
   => H.Component HH.HTML Query ParentInput Message m
component =
  H.lifecycleParentComponent
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

  render :: State -> H.ParentHTML Query (ChildQuery m) ChildSlot m
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

  renderEvalError :: forall a b c d. EvalError -> H.ParentHTML a b c d
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

  renderSection :: Section ExprType -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  renderSection (section) =
    Card.card_ $
      append
      [ Format.subHeading_ [ HH.text section.name ] ]
      $ renderField <$> section.contents

  renderField :: Field ExprType -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  renderField (field) =
    FormField.field_
      { label: HH.text $ print field.name
      , helpText: Just $ print field.description
      , error: Nothing
      , inputId: field.key
      }
      [ renderInput field ]

  renderInput :: Field ExprType -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  renderInput (field) = case field.input of
    Dropdown dropdown ->
      HH.slot'
        cp1
        field.key
        Dropdown.component
        { selectedItem: getValue dropdown
        , items: fold (toArray dropdown.options)
        , render:
          Dropdown.Render.render $
            Dropdown.Render.defDropdown
              Button.button
              []
              (foldMap (print <<< _.name) <<< toPair)
              (print field.description)
        }
        (HE.input $ DropdownQuery field.key)
    Text input ->
      Input.input
        [ HP.value $ fromMaybe "" $ toString =<< getValue input
        , HP.placeholder $ print input.placeholder
        , HP.id_ field.key
        ]
    Toggle input ->
      Toggle.toggle
        [ HP.checked $ fromMaybe false $ toBoolean =<< getValue input
        , HP.id_ field.key
        , HE.onChecked (HE.input $ UpdateKey field.key <<< Boolean)
        ]

eval :: forall m. Query ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Message m
eval = case _ of
  Initialize a -> a <$ do
    H.modify_ _ { values = Lynx.Data.Form.keys testPage }
    void $ bitraverse_
      (\e -> H.modify _ { form = Failure e })
      (\f -> eval $ EvalForm f a)
      (Right testPage)

  EvalForm expr a -> a <$ do
    { values } <- H.get
    let evaled' = Lynx.Data.Form.eval (\key -> Data.Map.lookup key values) expr
    evaled <- for evaled' \page -> do
      for_ page.contents \section -> do
        for_ section.contents \field -> do
          case field.input of
            Dropdown dropdown ->
              for_ (toArray dropdown.options) \options ->
                H.query' cp1 field.key (Dropdown.SetItems options unit)
            Text _ -> pure unit
            Toggle _ -> pure unit
      pure page
    H.modify_ _ { form = map { expr, evaled: _ } (fromEither evaled) }

  UpdateKey key val a -> do
    H.modify_ \state -> state { values = Data.Map.insert key val state.values }
    { form } <- H.get
    case form of
      Success { expr } ->
        eval (EvalForm (Lynx.Data.Form.setValue key val expr) a)
      _ -> pure a

  DropdownQuery key message a -> case message of
    Dropdown.Emit x -> a <$ eval x
    Dropdown.Selected val -> eval (UpdateKey key val a)
    Dropdown.VisibilityChanged _ -> pure a
