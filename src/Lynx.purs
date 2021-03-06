module Lynx where

import Prelude
import Data.Array as Data.Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (find, fold, foldMap, for_, length, sum)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.FunctorWithIndex as Data.FunctorWithIndex
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Data.Maybe
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty as Data.NonEmpty
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2, cp3)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as Halogen.HTML.Elements.Keyed
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Expr (EvalError(..), Expr, ExprType, Key, boolean_, cents_, datetime_, print, reflectType, string_, toArray, toBoolean, toCents, toDateTime, toObject, toPair, toString)
import Lynx.Form (Field, Input(..), InputSource(..), Page, Section, Sequence, Tab, TabSections(..), ValidationError(..), SequenceSection, asyncFromTypeahead, errors, errorsToArray, getValue, tabSections)
import Lynx.Form as Lynx.Form
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Header as Header
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.ItemContainer (boldMatches)
import Ocelot.Block.Layout as Layout
import Ocelot.Block.NavigationTab as NavigationTab
import Ocelot.Block.Toggle (toggle) as Toggle
import Ocelot.Component.DateTimePicker as DateTimePicker
import Ocelot.Component.Dropdown as Dropdown
import Ocelot.Component.Dropdown.Render as Dropdown.Render
import Ocelot.Component.Typeahead as Typeahead
import Ocelot.Data.Currency (parseCentsFromDollarStr)
import Ocelot.HTML.Properties (css)
import URI (Fragment)
import URI.Fragment as URI.Fragment

type State
  = { activeTab :: String
    , evaled :: Either EvalError (Page ExprType)
    , expr :: Page Expr
    , fragment :: Fragment
    , idGenerator :: Aff String
    , values :: Map Key ExprType
    }

data Query a
  = EvalForm (Page Expr) a
  | HandleInput ParentInput a
  | UpdateValue Key (InputSource ExprType) a
  | DropdownQuery Key (Dropdown.Message Query ExprType) a
  | DateTimePickerQuery Key DateTimePicker.Message a
  | TypeaheadSingleQuery Key (Typeahead.Message Query Maybe ExprType) a
  | AddSection Key a
  | RemoveSection Key Int a
  | Cancel a
  | Submit a

type ParentInput
  = { activeTab :: String
    , expr :: Page Expr
    , fragment :: Fragment
    , idGenerator :: Aff String
    }

type ChildQuery m
  = Dropdown.Query Query ExprType m
      <\/> DateTimePicker.Query
      <\/> Typeahead.Query Query Maybe ExprType m
      <\/> Const Void

type ChildSlot
  = Key
      \/ Key
      \/ Key
      \/ Void

data Message
  = Canceled
  | Submitted { expr :: Page Expr }

component ::
  ∀ m.
  MonadAff m =>
  H.Component HH.HTML Query ParentInput Message m
component =
  H.parentComponent
    { initialState
    , eval
    , render
    , receiver: HE.input HandleInput
    }
  where
  render :: State -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  render { activeTab, evaled, fragment, values } =
    HH.div_
      [ case evaled of
          Left e ->
            Layout.section_
              [ Card.card_
                  [ Format.p
                      [ css "text-red" ]
                      [ renderEvalError e ]
                  ]
              ]
          Right page ->
            HH.div_
              [ Header.stickyFormHeader
                  { brand: Just "https://citizennet.com/manager/images/logo.svg"
                  , name: [ HH.text page.name ]
                  , title: []
                  , buttons:
                    [ HH.a
                        [ HE.onClick (HE.input_ Cancel)
                        , css "cursor-pointer no-underline text-grey-70 mr-6"
                        ]
                        [ HH.text "Cancel" ]
                    , Button.buttonPrimary
                        [ HE.onClick (HE.input_ Submit) ]
                        [ HH.text "Submit" ]
                    ]
                  }
                  { activePage: activeTab
                  , tabs: Data.Array.fromFoldable (map (fromTab fragment) page.tabs)
                  }
              , Layout.grid_
                  [ renderTab activeTab page.tabs
                  , Layout.side_ []
                  ]
              ]
      , HH.pre_ [ HH.text $ show values ]
      ]

  renderEvalError :: forall a b c d. EvalError -> H.ParentHTML a b c d
  renderEvalError = case _ of
    IfCondition x ->
      HH.text
        $ "Expected conditional to be a Boolean, but its type is: "
        <> reflectType x
    EqualMismatch x ->
      HH.text
        $ "Expected both sides of equal to have the same type,"
        <> " but they are different."
        <> " left: "
        <> reflectType x.left
        <> " right: "
        <> reflectType x.right

  renderTab :: String -> NonEmpty Array (Tab ExprType) -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  renderTab activeTab tabs = Layout.main_ (Data.Array.fromFoldable $ (tabSections renderSection renderSequence) <$> tab.sections)
    where
    byLink :: Tab ExprType -> Boolean
    byLink { link } = activeTab == link

    tab :: Tab ExprType
    tab = fromMaybe (Data.NonEmpty.head tabs) (find byLink tabs)

  renderSection :: Section ExprType -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  renderSection section =
    Card.card_
      $ append [ Format.subHeading_ [ HH.text section.name ] ]
      $ Data.Array.fromFoldable
      $ renderField
      <$> section.fields

  renderSequence :: Sequence ExprType -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  renderSequence sequence =
    Layout.container_
      ( [ Format.subHeading_ [ HH.text sequence.name ]
        , Halogen.HTML.Elements.Keyed.div_ case sequence.values of
            UserInput sections ->
              Data.FunctorWithIndex.mapWithIndex
                (renderSequenceSection sequence.key)
                sections
            Invalid sections ->
              Data.FunctorWithIndex.mapWithIndex
                (renderSequenceSection sequence.key)
                sections
            _ -> mempty
        , Button.button
            [ HE.onClick (HE.input_ $ AddSection sequence.key)
            ]
            [ Icon.plus_
            , HH.span [ css "pl-2" ] [ HH.text "Add" ]
            ]
        ]
      )

  renderSequenceSection :: Key -> Int -> SequenceSection ExprType -> Tuple String (H.ParentHTML Query (ChildQuery m) ChildSlot m)
  renderSequenceSection key index section =
    Tuple
      section.id
      ( Card.card_
          ( [ Format.subHeading
                [ css "relative"
                ]
                [ HH.text section.name
                , Button.buttonClear
                    [ css "absolute pin-r pr-0"
                    , HE.onClick (HE.input_ $ RemoveSection key index)
                    ]
                    [ Icon.close_
                    ]
                ]
            ]
              <> Data.Array.fromFoldable (map (renderField <<< indexKey index) section.fields)
          )
      )

  indexKey :: forall r. Int -> { key :: Key | r } -> { key :: Key | r }
  indexKey index r = r { key = r.key <> show index }

  renderField :: Field ExprType -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  renderField field =
    FormField.field_
      { label: HH.text $ print field.name
      , helpText: [ HH.text $ print field.description ]
      , error: renderValidation field.input
      , inputId: field.key
      }
      [ renderInput field ]

  renderInput :: Field ExprType -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  renderInput field = case field.input of
    Currency currency ->
      Input.currency_
        [ HP.id_ field.key
        , HP.placeholder (print currency.placeholder)
        , HP.value
            $ fold do
                value <- getValue currency
                _ <- toCents value
                pure (print value)
        , HE.onValueChange (HE.input $ UpdateValue field.key <<< Data.Maybe.maybe UserCleared (UserInput <<< cents_) <<< parseCentsFromDollarStr)
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
      HH.slot' cp1 field.key Dropdown.component
        { selectedItem: getValue dropdown
        , items: fold (toArray dropdown.options)
        , render:
          Dropdown.Render.render
            $ Dropdown.Render.defDropdown
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
        , HE.onValueChange (HE.input $ UpdateValue field.key <<< UserInput <<< string_)
        ]
    Toggle input ->
      Toggle.toggle
        [ HP.checked $ Data.Maybe.fromMaybe false $ toBoolean =<< getValue input
        , HP.id_ field.key
        , HE.onChecked (HE.input $ UpdateValue field.key <<< UserInput <<< boolean_)
        ]
    TypeaheadSingle typeahead ->
      HH.slot'
        cp3
        field.key
        Typeahead.single
        ( Typeahead.asyncSingle
            { async: asyncFromTypeahead typeahead
            , itemToObject: toObject
            , renderFuzzy: HH.span_ <<< boldMatches "value"
            }
            []
        )
        (HE.input $ TypeaheadSingleQuery field.key)

  renderValidation ::
    Input ExprType ->
    Array (H.ParentHTML Query (ChildQuery m) ChildSlot m)
  renderValidation = case _ of
    Currency currency -> renderValidation' currency.errors
    DateTime dateTime -> renderValidation' dateTime.errors
    Dropdown dropdown -> renderValidation' dropdown.errors
    Text text -> renderValidation' text.errors
    Toggle toggle -> renderValidation' toggle.errors
    TypeaheadSingle typeahead -> renderValidation' typeahead.errors
    where
    renderValidation' =
      errorsToArray
        >>> case _ of
            [] -> []
            [ e ] -> [ HH.p_ [ HH.text $ renderValidationError e ] ]
            es -> [ HH.ul_ $ HH.li_ <<< pure <<< HH.text <<< renderValidationError <$> es ]

    renderValidationError = case _ of
      Required -> "This field is required"
      MinLength x -> "Must contain at least " <> show x <> " characters"
      MaxLength x -> "Cannot contain more than " <> show x <> " characters"
      InvalidOption x -> x <> " is not a valid option"

eval ::
  ∀ m.
  MonadAff m =>
  Query
    ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Message m
eval = case _ of
  EvalForm expr a -> do
    let
      values = Lynx.Form.keys expr

      evaled = Lynx.Form.eval (\key -> Data.Map.lookup key values) expr

      evalField field = do
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

      evalSection section = for_ section.fields evalField

      evalSequenceSection sequenceSection = for_ sequenceSection.fields evalField
    for_ evaled \page -> do
      for_ page.tabs \tab -> do
        for_ tab.sections \sections' -> do
          case sections' of
            TabSection section -> evalSection section
            TabSequence sequence -> case sequence.values of
              UserInput sections -> for_ sections evalSequenceSection
              Invalid sections -> for_ sections evalSequenceSection
              _ -> pure unit
    H.modify_ _ { evaled = evaled, expr = expr, values = values }
    pure a
  HandleInput { activeTab, expr, fragment } a -> do
    { fragment: oldFragment } <- H.get
    H.modify_ _ { activeTab = activeTab, fragment = fragment }
    case fragment == oldFragment of
      true -> pure a
      false -> eval (EvalForm expr a)
  UpdateValue key val a -> do
    { expr } <- H.get
    eval (EvalForm (Lynx.Form.setValue key val expr) a)
  DropdownQuery key message a -> case message of
    Dropdown.Emit x -> a <$ eval x
    Dropdown.Selected val -> eval (UpdateValue key (UserInput val) a)
    Dropdown.VisibilityChanged _ -> pure a
  DateTimePickerQuery key message a -> case message of
    DateTimePicker.DateMessage _ -> pure a
    DateTimePicker.SelectionChanged (Just val) -> eval (UpdateValue key (UserInput $ datetime_ val) a)
    DateTimePicker.SelectionChanged Nothing -> eval (UpdateValue key UserCleared a)
    DateTimePicker.TimeMessage _ -> pure a
  TypeaheadSingleQuery key message a -> case message of
    Typeahead.Emit x -> a <$ eval x
    Typeahead.Searched _ -> pure a
    Typeahead.Selected val -> eval (UpdateValue key (UserInput val) a)
    Typeahead.SelectionChanged _ _ -> pure a
  AddSection key a -> do
    { idGenerator, expr: expr' } <- H.get
    expr <- H.liftAff (Lynx.Form.stamp idGenerator key expr')
    eval (EvalForm expr a)
  RemoveSection key index a -> do
    { expr } <- H.get
    eval (EvalForm (Lynx.Form.unstamp key index expr) a)
  Cancel a -> do
    H.raise Canceled
    pure a
  Submit a -> do
    { expr } <- H.get
    H.raise (Submitted { expr })
    pure a

fromTab :: forall a. Fragment -> Tab a -> NavigationTab.Tab String
fromTab fragment { sections: sections'', name, link } =
  { errors:
    sum do
      sections' <- Data.Array.fromFoldable sections''
      field <- case sections' of
        TabSection { fields } -> Data.Array.fromFoldable fields
        TabSequence { values } -> foldMap (_ >>= Data.Array.fromFoldable <<< _.fields) values
      pure (length $ errorsToArray $ errors field)
  , name
  , link: URI.Fragment.print fragment <> "/" <> link
  , page: link
  }

initialState :: ParentInput -> State
initialState { activeTab, expr, fragment, idGenerator } =
  { activeTab
  , evaled
  , expr
  , fragment
  , idGenerator
  , values
  }
  where
  evaled :: Either EvalError (Page ExprType)
  evaled = Lynx.Form.eval (\key -> Data.Map.lookup key values) expr

  values :: Map String ExprType
  values = Lynx.Form.keys expr
