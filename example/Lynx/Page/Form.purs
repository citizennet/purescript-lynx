module Lynx.Page.Form where

import Prelude hiding ((/))

import Data.Const (Const)
import Data.Either.Nested (type (\/))
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.NonEmpty as Data.NonEmpty
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log, logShow)
import Halogen as H
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lynx as Lynx
import Lynx.Form (mvpPage, testPage)
import Routing.Duplex (RouteDuplex', segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import URI (Fragment)
import URI.Fragment as URI.Fragment

data Route
  = MVP String
  | Profile1

derive instance eqRoute :: Eq Route

derive instance genericRoute :: Generic Route _

derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec =
  sum
    { "MVP": URI.Fragment.toString mvp / string segment
    , "Profile1": URI.Fragment.toString profile1 / noArgs
    }

mvp :: Fragment
mvp = URI.Fragment.fromString "mvp"

profile1 :: Fragment
profile1 = URI.Fragment.fromString "profile-1"

type State
  = { fragment :: Fragment
    , idGenerator :: Aff String
    , route :: Route
    }

data Query a
  = Initialize ParentInput a
  | LynxQuery Lynx.Message a

type ParentInput
  = { fragment :: Fragment
    , idGenerator :: Aff String
    , route :: Route
    }

type ChildQuery
  = Lynx.Query
      <\/> Const Void

type ChildSlot
  = Unit
      \/ Void

type Message
  = Void

component ::
  ∀ m.
  MonadAff m =>
  H.Component HH.HTML Query ParentInput Message m
component =
  H.parentComponent
    { initialState
    , eval
    , render
    , receiver: HE.input Initialize
    }
  where
  initialState :: ParentInput -> State
  initialState = identity

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { fragment, idGenerator, route } = HH.slot' cp1 unit Lynx.component input (HE.input LynxQuery)
    where
    appendFragment :: Fragment -> Fragment
    appendFragment segment =
      URI.Fragment.fromString
        (URI.Fragment.toString fragment <> "/" <> URI.Fragment.toString segment)

    input :: Lynx.ParentInput
    input = case route of
      MVP activeTab ->
        { activeTab
        , expr: mvpPage
        , fragment: appendFragment mvp
        , idGenerator
        }
      Profile1 ->
        { activeTab: (Data.NonEmpty.head testPage.tabs).link
        , expr: testPage
        , fragment: appendFragment profile1
        , idGenerator
        }

eval ::
  ∀ m.
  MonadAff m =>
  Query
    ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
eval = case _ of
  Initialize { fragment, route } a -> do
    H.modify_ _ { fragment = fragment, route = route }
    pure a
  LynxQuery message a ->
    case message of
      Lynx.Canceled -> pure a
      Lynx.Submitted { expr } -> do
        log "Form submitted:"
        logShow expr
        pure a
