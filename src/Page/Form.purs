module Lynx.Page.Form where

import Prelude

import Data.Bifoldable (bifor_)
import Data.Const (Const)
import Data.Either.Nested (type (\/))
import Data.Foldable (fold, foldMap, for_)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as Foreign.Object
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2, cp3)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Data.Expr (EvalError(..), Expr, ExprType, Key, datetime_, print, reflectType, toArray, toBoolean, toCents, toDateTime, toPair, toString)
import Lynx.Data.Expr as Lynx.Data.Expr
import Lynx.Data.Form (Field, Input(..), InputSource(..), Page, Section, getValue, mvpPage, testPage, userInput)
import Lynx.Data.Form as Lynx.Data.Form
import Network.RemoteData (RemoteData(..), fromEither)
import Network.RemoteData as Network.RemoteData
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Block.ItemContainer (boldMatches)
import Ocelot.Block.Layout as Layout
import Ocelot.Block.Toggle (toggle) as Toggle
import Ocelot.Component.DateTimePicker as DateTimePicker
import Ocelot.Component.Dropdown as Dropdown
import Ocelot.Component.Dropdown.Render as Dropdown.Render
import Ocelot.Component.Typeahead as Typeahead
import Ocelot.HTML.Properties (css)
import Routing.Duplex (RouteDuplex', path)
import Routing.Duplex.Generic (noArgs, sum)

data Route
  = MVP
  | Profile1

derive instance eqRoute :: Eq Route

derive instance genericRoute :: Generic Route _

derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = sum
  { "MVP": path "mvp" noArgs
  , "Profile1": path "profile-1" noArgs
  }

type State =
  { form :: RemoteData EvalError { expr :: Page Expr, evaled :: Page ExprType }
  , route :: Route
  , values :: Map Key ExprType
  }

data Query a
  = Initialize a
  | EvalForm (Page Expr) a
  | UpdateKey Key (InputSource ExprType) a
  | DropdownQuery Key (Dropdown.Message Query ExprType) a
  | DateTimePickerQuery Key DateTimePicker.Message a
  | TypeaheadSingleQuery  Key (Typeahead.Message Query Maybe ExprType) a

type ParentInput = Route

type ChildQuery m
  = Dropdown.Query Query ExprType  m
  <\/> DateTimePicker.Query
  <\/> Typeahead.Query Query Maybe ExprType m
  <\/> Const Void

type ChildSlot
  = Key
  \/ Key
  \/ Key
  \/ Void

type Message = Void

component
  :: ∀ m
   .  MonadAff m
   => H.Component HH.HTML Query ParentInput Message m
component =
  H.lifecycleParentComponent
    { initialState
    , eval
    , render
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where

  initialState :: ParentInput -> State
  initialState = case _ of
    route ->
      { form: Loading
      , route
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
    Currency currency ->
      Input.currency_
        [ HP.id_ field.key
        , HP.placeholder (print currency.placeholder)
        , HP.value $ fold do
          value <- getValue currency
          _ <- toCents value
          pure (print value)
        ]
    DateTime dateTime ->
      HH.slot'
        cp2
        field.key
        DateTimePicker.component
        { selection: toDateTime =<< getValue dateTime
        , targetDate: Nothing
        }
        (HE.input $ DateTimePickerQuery field.key)
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
              (print dropdown.placeholder)
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
        , HE.onChecked (HE.input $ UpdateKey field.key <<< UserInput <<< Lynx.Data.Expr.Boolean)
        ]
    TypeaheadSingle typeahead ->
      HH.slot'
        cp3
        field.key
        Typeahead.single
        ( Typeahead.syncSingle
          { itemToObject: \item -> fold do
            { name: name', value: value' } <- toPair item
            name <- toString name'
            value <- toString value'
            pure (Foreign.Object.fromHomogeneous { name, value })
          , renderFuzzy: HH.span_ <<< boldMatches "value"
          }
          []
        )
        { items = Network.RemoteData.fromMaybe (toArray typeahead.options)
        }
        (HE.input $ TypeaheadSingleQuery field.key)

eval :: forall m. Query ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Message m
eval = case _ of
  Initialize a -> a <$ do
    { route } <- H.get
    page' <- case route of
      MVP -> pure (Success mvpPage)
      Profile1 -> pure (Success testPage)
    bifor_ page' (\e -> H.modify _ { form = Failure e }) \page -> do
      H.modify_ _ { values = Lynx.Data.Form.keys page }
      eval (EvalForm page a)

  EvalForm expr a -> a <$ do
    { values } <- H.get
    let evaled' = Lynx.Data.Form.eval (\key -> Data.Map.lookup key values) expr
    evaled <- for evaled' \page -> do
      for_ page.contents \section -> do
        for_ section.contents \field -> do
          case field.input of
            Currency _ -> pure unit
            DateTime _ -> pure unit
            Dropdown dropdown ->
              for_ (toArray dropdown.options) \options ->
                H.query' cp1 field.key (Dropdown.SetItems options unit)
            Text _ -> pure unit
            Toggle _ -> pure unit
            TypeaheadSingle typeahead ->
              for_ (toArray typeahead.options) \options ->
                H.query' cp3 field.key (Typeahead.ReplaceItems (pure options) unit)
      pure page
    H.modify_ _ { form = map { expr, evaled: _ } (fromEither evaled) }

  UpdateKey key val a -> do
    H.modify_ \state ->
      state { values = Data.Map.alter (\_ -> userInput val) key state.values }
    { form } <- H.get
    case form of
      Success { expr } ->
        eval (EvalForm (Lynx.Data.Form.setValue key val expr) a)
      _ -> pure a

  DropdownQuery key message a -> case message of
    Dropdown.Emit x -> a <$ eval x
    Dropdown.Selected val -> eval (UpdateKey key (UserInput val) a)
    Dropdown.VisibilityChanged _ -> pure a

  DateTimePickerQuery key message a -> case message of
    DateTimePicker.DateMessage _ -> pure a
    DateTimePicker.SelectionChanged (Just val) ->
      eval (UpdateKey key (UserInput $ datetime_ val) a)
    DateTimePicker.SelectionChanged Nothing ->
      eval (UpdateKey key UserCleared a)
    DateTimePicker.TimeMessage _ -> pure a

  TypeaheadSingleQuery key message a -> case message of
    Typeahead.Emit x -> a <$ eval x
    Typeahead.Searched _ -> pure a
    Typeahead.Selected val -> eval (UpdateKey key (UserInput val) a)
    Typeahead.SelectionChanged _ _ -> pure a
