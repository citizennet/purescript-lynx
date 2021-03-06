module Lynx.Page.Home where

import Prelude
import Data.Maybe (Maybe(..))
import Data.NonEmpty as Data.NonEmpty
import Halogen as H
import Halogen.HTML as HH
import Lynx.Form (mvpPage)
import Lynx.Page.Form as Lynx.Page.Form
import Lynx.Route (Route(..))
import Lynx.Util.HTML (safeHref)
import Ocelot.Block.Card as Card
import Ocelot.Block.Format as Format
import Ocelot.Block.Table as Table

type State
  = Unit

data Query a
  = Noop a

type Input
  = Unit

type Message
  = Void

type Form
  = { name :: String, id :: Lynx.Page.Form.Route }

forms :: Array Form
forms =
  [ { name: "Profile", id: Lynx.Page.Form.Profile1 }
  , { name: "MVP", id: Lynx.Page.Form.MVP (Data.NonEmpty.head mvpPage.tabs).link }
  ]

type List
  = { name :: String }

lists :: Array List
lists =
  [ { name: "MVP" }
  ]

component ::
  ∀ m.
  H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const unit
    , eval
    , render
    , receiver: const Nothing
    }
  where
  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (Noop a) = pure a

  render :: State -> H.ComponentHTML Query
  render _ =
    Card.card_
      [ Format.heading_ [ HH.text "Forms" ]
      , Table.table_ $ renderRow <$> forms
      , Format.heading_ [ HH.text "Lists" ]
      , Table.table_ (map renderList lists)
      ]

  renderList :: List -> H.ComponentHTML Query
  renderList { name } =
    Table.row_
      [ Table.cell_
          [ HH.a
              [ safeHref List ]
              [ HH.text name ]
          ]
      ]

  renderRow :: Form -> H.ComponentHTML Query
  renderRow { name, id } =
    Table.row_
      [ Table.cell_
          [ HH.a
              [ safeHref $ Form id ]
              [ HH.text name ]
          ]
      ]
