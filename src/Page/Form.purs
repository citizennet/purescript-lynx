module Lynx.Page.Form where

import Prelude

import Data.Const (Const)
import Data.Either.Nested (type (\/))
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lynx as Lynx
import Lynx.Data.Form (mvpPage, testPage)
import Routing.Duplex (RouteDuplex', path, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
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
routeCodec = sum
  { "MVP": path "mvp" (string segment)
  , "Profile1": path "profile-1" noArgs
  }

type State =
  { route :: Route
  }

data Query a
  = Initialize Route a
  | LynxQuery Lynx.Message a

type ParentInput = Route

type ChildQuery
  = Lynx.Query
  <\/> Const Void

type ChildSlot
  = Unit
  \/ Void

type Message = Void

component
  :: ∀ m
   .  MonadAff m
   => H.Component HH.HTML Query ParentInput Message m
component =
  H.parentComponent
    { initialState
    , eval
    , render
    , receiver: HE.input Initialize
    }
  where

  initialState :: ParentInput -> State
  initialState = case _ of
    route ->
      { route
      }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route } =
    HH.slot' cp1 unit Lynx.component input (HE.input LynxQuery)
    where
    input :: Lynx.ParentInput
    input = case route of
      MVP activeTab ->
        { activeTab
        , expr: mvpPage
        , fragment: URI.Fragment.fromString "form/mvp"
        }
      Profile1 ->
        { activeTab: "user"
        , expr: testPage
        , fragment: URI.Fragment.fromString "form/profile-1"
        }

eval
  :: ∀ m
   . MonadAff m
  => Query
  ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
eval = case _ of
  Initialize route a -> do
    H.modify_ _ { route = route }
    pure a
  LynxQuery message _ -> absurd message
