module Formal.Util.HTML where

import Prelude

import Formal.Route (Route, routeCodec)
import Halogen as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

safeHref :: ∀ r i. Route -> HH.IProp ( href :: String | r ) i
safeHref = HP.href <<< append "#" <<< print routeCodec
