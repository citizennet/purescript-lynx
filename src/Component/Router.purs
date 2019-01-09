module Formal.Component.Router where

import Prelude

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formal.Page.Form as Form
import Formal.Page.Home as Home
import Formal.Route (Route(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH

type State = Route

data Query a = Navigate Route a

type Input = Unit

type Message = Void

type ChildSlots = Either2 Unit Unit

type ChildQueries = Coproduct2 Home.Query Form.Query

component
  :: ∀ m
   . MonadAff m
  => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState: const Home
    , eval
    , render
    , receiver: const Nothing
    }
  where

  eval :: Query ~> H.ParentDSL State Query ChildQueries ChildSlots Message m
  eval (Navigate dest a) = do
    route <- H.get
    when (route /= dest) do
      H.put dest
    pure a

  render :: State -> H.ParentHTML Query ChildQueries ChildSlots m
  render = case _ of
    Home -> HH.slot' CP.cp1 unit Home.component unit absurd
    Form s -> HH.slot' CP.cp2 unit Form.component s absurd
