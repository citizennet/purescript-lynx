module Test.Main where

import Prelude

import Effect (Effect)
import Test.Data.Expr as Test.Data.Expr
import Test.Lynx.Data.ExprType as Test.Lynx.Data.ExprType
import Test.Lynx.Data.Form as Test.Lynx.Data.Form
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Test.Data.Expr.suite
  Test.Lynx.Data.ExprType.suite
  Test.Lynx.Data.Form.suite
