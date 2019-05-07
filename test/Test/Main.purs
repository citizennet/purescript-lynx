module Test.Main where

import Prelude

import Effect (Effect)
import Test.Lynx.Expr as Test.Lynx.Expr
import Test.Lynx.Form as Test.Lynx.Form
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Test.Lynx.Expr.suite
  Test.Lynx.Form.suite
