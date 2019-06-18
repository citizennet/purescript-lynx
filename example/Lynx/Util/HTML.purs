module Lynx.Util.HTML where

import Prelude
import Halogen as HH
import Halogen.HTML.Properties as HP
import Lynx.Route (Route, routeCodec)
import Routing.Duplex (print)

safeHref :: ∀ r i. Route -> HH.IProp ( href :: String | r ) i
safeHref = HP.href <<< append "#" <<< print routeCodec
