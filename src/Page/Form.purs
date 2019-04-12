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
import Lynx.Data.Expr (Expr)
import Lynx.Data.Form (Page, mvpPage, testPage)
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
    HH.slot' cp1 unit Lynx.component (pure page) (HE.input LynxQuery)
    where
    page :: Page Expr
    page = case route of
      MVP -> mvpPage
      Profile1 -> testPage

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
