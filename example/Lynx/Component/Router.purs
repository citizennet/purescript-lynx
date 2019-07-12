module Lynx.Component.Router where

import Prelude
import Data.Array (catMaybes)
import Data.DateTime (DateTime(..), Time(..), canonicalDate)
import Data.Either (Either, hush)
import Data.Either.Nested (Either3)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..), fromJust)
import Data.UUID as Node.UUID
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lynx.Expr as Lynx.Expr
import Lynx.Form as Lynx.Form
import Lynx.List as Lynx.List
import Lynx.Page.Form as Form
import Lynx.Page.Home as Home
import Lynx.Route (Route(..), form)
import Partial.Unsafe (unsafePartial)
import URI.Fragment as URI.Fragment

type State
  = Route

data Query a
  = Navigate Route a

type Input
  = Route

type Message
  = Void

type ChildSlots
  = Either3 Unit Unit Unit

type ChildQueries
  = Coproduct3 Home.Query Form.Query Lynx.List.Query

component ::
  ∀ m.
  MonadAff m =>
  H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState: const Home
    , eval
    , render
    , receiver: HE.input Navigate
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
    Form s ->
      HH.slot'
        CP.cp2
        unit
        Form.component
        { fragment: URI.Fragment.fromString ("/" <> URI.Fragment.toString form)
        , idGenerator: H.liftEffect (map show Node.UUID.genUUID)
        , route: s
        }
        absurd
    List ->
      HH.slot'
        CP.cp3
        unit
        Lynx.List.component
        { columns:
          [ { name: Lynx.Expr.val_ (Lynx.Expr.string_ "Name")
            , value: "name"
            , width: Lynx.List.Large
            }
          , { name: Lynx.Expr.val_ (Lynx.Expr.string_ "Start")
            , value: "start"
            , width: Lynx.List.Small
            }
          ]
        , rows:
          catMaybes
            $ map hush
                [ hydrateMvp "Test Order 1" (unsafeDateTime 2019 6 6)
                , hydrateMvp "One More Just for Fun" (unsafeDateTime 2019 7 1)
                , hydrateMvp "This is the Third" (unsafeDateTime 2019 7 3)
                ]
        }
        absurd

  hydrateMvp :: String -> DateTime -> Either Lynx.Expr.EvalError (Lynx.Form.Page Lynx.Expr.ExprType)
  hydrateMvp name date =
    ( Lynx.Form.eval (const Nothing)
        <<< Lynx.Form.setValue "start" (Lynx.Form.UserInput $ Lynx.Expr.datetime_ date)
        <<< Lynx.Form.setValue "name" (Lynx.Form.UserInput $ Lynx.Expr.string_ name)
    )
      Lynx.Form.mvpPage

  unsafeDateTime :: Int -> Int -> Int -> DateTime
  unsafeDateTime y m d = do
    let
      force :: ∀ a. BoundedEnum a => Int -> a
      force = unsafePartial fromJust <<< toEnum

      date = canonicalDate (force y) (force m) (force d)

      time = Time (force 0) (force 0) (force 0) (force 0)
    DateTime date time
