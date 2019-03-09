module Test.Lynx.Page.Form (suite) where

import Prelude

import Control.Monad.State (evalStateT)
import Control.Monad.State.Class (get)
import Control.Monad.Trans.Class (lift)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Lynx.Data.Expr (ExprType(..))
import Lynx.Page.Form (Query(..), State, eval)
import Network.RemoteData (RemoteData(..))
import Ocelot.Component.Dropdown as Dropdown
import Select as Select
import Test.Unit (TestSuite, test)
import Test.Unit as Test.Unit
import Test.Unit.Assert (assert, equal)

suite :: TestSuite
suite = Test.Unit.suite "Test.Lynx.Page.Form" do
  let initialState :: State
      initialState =
        { form: NotAsked
        , values: mempty
        }

  Test.Unit.suite "eval" do
    Test.Unit.suite "DropdownQuery" do
      Test.Unit.suite "Emit" do
        test "threads queries through appropriately" $ flip evalStateT initialState do
          eval (DropdownQuery "" (Dropdown.Emit $ Initialize unit) unit)

          state <- get
          lift (assert "State should have changed" (initialState /= state))

      Test.Unit.suite "Selected" do
        test "causes the state to be updated" $ flip evalStateT initialState do
          let selected :: ExprType
              selected = Pair { name: String "Cherry", value: String "Cherry" }

          eval (Initialize unit)
          eval (DropdownQuery "food" (Dropdown.Selected selected) unit)

          { values } <- get
          let actual :: Maybe ExprType
              actual = Data.Map.lookup "food" values
              expected :: ExprType
              expected = selected
          lift (equal (Just expected) actual)

      Test.Unit.suite "VisibilityChanged" do
        Test.Unit.suite "On" do
          test "does nothing" $ flip evalStateT initialState do
            eval (DropdownQuery "" (Dropdown.VisibilityChanged Select.On) unit)

            state <- get
            lift (assert "State should not have changed" (initialState == state))

        Test.Unit.suite "On" do
          test "does nothing" $ flip evalStateT initialState do
            eval (DropdownQuery "" (Dropdown.VisibilityChanged Select.Off) unit)

            state <- get
            lift (assert "State should not have changed" (initialState == state))
