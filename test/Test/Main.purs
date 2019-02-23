module Test.Main where

import Prelude

import Effect (Effect)
import Test.Lynx.Data.Form as Test.Lynx.Data.Form
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Test.Lynx.Data.Form.suite
