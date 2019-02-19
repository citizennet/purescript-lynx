module Lynx.Page.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Lynx.Route (Route(..))
import Lynx.Util.HTML (safeHref)
import Ocelot.Block.Card as Card
import Ocelot.Block.Format as Format
import Ocelot.Block.Table as Table

type State = Unit

data Query a = Noop a

type Input = Unit

type Message = Void

type Form = { name :: String, id :: String }

forms :: Array Form
forms =
  [ { name: "Profile", id: "profile-1" }
  ]

component
  :: ∀ m
   . H.Component HH.HTML Query Input Message m
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
