module Lynx.Page.Form where

import Prelude

import Data.Bifoldable (bifor_)
import Data.Either.Nested (Either1)
import Data.Foldable (fold, foldMap, for_)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Traversable (for)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Data.Expr (EvalError(..), Expr(..), ExprType(..), Key, print, reflectType, toArray, toBoolean, toCents, toPair, toString)
import Lynx.Data.Form (Field, Input(..), InputSource(..), Page, Section, ValidationError(..), errorsToArray, getValue, mvpPage, testPage)
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
import Ocelot.Data.Currency (parseCentsFromDollarStr)
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
  | UpdateValue Key (InputSource Expr) a
  | DropdownQuery Key (Dropdown.Message Query ExprType) a

type ParentInput = Route

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
  render { form, values } =
    HH.div_
      [ Layout.section_
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
      , HH.pre_ [ HH.text $ show values ]
      ]

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
      , helpText: [ HH.text $ print field.description ]
      , error: renderValidation field.input
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
        , HE.onValueChange (HE.input $ UpdateValue field.key <<< Data.Maybe.maybe UserCleared (UserInput <<< Val <<< Cents) <<< parseCentsFromDollarStr)
        ]
    Dropdown dropdown ->
      HH.slot' cp1 field.key Dropdown.component
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
        [ HP.value $ Data.Maybe.fromMaybe "" $ toString =<< getValue input
        , HP.placeholder $ print input.placeholder
        , HP.id_ field.key
        , HE.onValueChange (HE.input $ UpdateValue field.key <<< UserInput <<< Val <<< String)
        ]
    Toggle input ->
      Toggle.toggle
        [ HP.checked $ Data.Maybe.fromMaybe false $ toBoolean =<< getValue input
        , HP.id_ field.key
        , HE.onChecked (HE.input $ UpdateValue field.key <<< UserInput <<< Val <<< Boolean)
        ]

  renderValidation
    :: Input ExprType
    -> Array (H.ParentHTML Query (ChildQuery m) ChildSlot m)
  renderValidation = case _ of
    Currency currency -> renderValidation' currency.errors
    Dropdown dropdown -> renderValidation' dropdown.errors
    Text text -> renderValidation' text.errors
    Toggle toggle -> renderValidation' toggle.errors
    where
    renderValidation' = errorsToArray >>> case _ of
      []  -> []
      [e] -> [ HH.p_ [ HH.text $ renderValidationError e ] ]
      es  -> [ HH.ul_ $ HH.li_ <<< pure <<< HH.text <<< renderValidationError <$> es ]

    renderValidationError = case _ of
      Required -> "This field is required"
      MinLength x -> "Must contain at least " <> show x <> " characters"
      MaxLength x -> "Cannot contain more than " <> show x <> " characters"
      InvalidOption x -> x <> " is not a valid option"

eval
  :: ∀ m
   . MonadAff m
  => Query
  ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Message m
eval = case _ of
  Initialize a -> a <$ do
    { route } <- H.get
    page' <- case route of
      MVP -> pure (Success mvpPage)
      Profile1 -> pure (Success testPage)
    void $ bifor_ page'
      do \e -> H.modify _ { form = Failure e }
      do \f -> eval (EvalForm f a)

  EvalForm expr a -> a <$ do
    let values = Lynx.Data.Form.keys expr
    let evaled' = Lynx.Data.Form.eval (\key -> Data.Map.lookup key values) expr
    evaled <- for evaled' \page -> do
      for_ page.contents \section -> do
        for_ section.contents \field -> do
          case field.input of
            Currency _ -> pure unit
            Dropdown dropdown ->
              for_ (toArray dropdown.options) \options -> do
                void $ H.query' cp1 field.key (Dropdown.SetItems options unit)
            Text _ -> pure unit
            Toggle _ -> pure unit
      pure page
    H.modify_ _
      { form = map { expr, evaled: _ } (fromEither evaled)
      , values = values
      }

  UpdateValue key val a -> do
    { values, form } <- H.get
    case form of
      Success { expr } -> do
        eval $ EvalForm (Lynx.Data.Form.setValue key val expr) a
      _ -> pure a

  DropdownQuery key message a -> case message of
    Dropdown.Emit x -> a <$ eval x
    Dropdown.Selected val -> eval (UpdateValue key (UserInput $ Val val) a)
    Dropdown.VisibilityChanged _ -> pure a
