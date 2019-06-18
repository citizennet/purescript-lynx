module Lynx.Route where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Lynx.Page.Form as Lynx.Page.Form
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import URI (Fragment)
import URI.Fragment as URI.Fragment

data Route
  = Home
  | Form Lynx.Page.Form.Route

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "Home": noArgs
        , "Form": URI.Fragment.toString form / Lynx.Page.Form.routeCodec
        }

form :: Fragment
form = URI.Fragment.fromString "form"
