module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Formal.AppM (runAppM)
import Formal.Component.Router as Router
import Formal.Route (routeCodec)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  let router :: H.Component HH.HTML Router.Query Unit Void Aff
      router = H.hoist (runAppM {}) Router.component

  driver <- runUI router unit body

  void $ H.liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ driver.query $ Router.Navigate new unit
