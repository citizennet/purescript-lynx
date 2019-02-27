module Lynx.Data.Expr where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, maybe)

type Key = String

data ExprType
  = Boolean Boolean
  | Int Int
  | String String

derive instance genericExprType :: Generic ExprType _

instance showExprType :: Show ExprType where
  show = genericShow

data Expr
  = Val ExprType
  | If Expr Expr Expr
  | Equal Expr Expr
  | Lookup Key

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

data EvalError
  = MissingKey Key
  | IfCondition ExprType
  | EqualMismatch { left :: ExprType, right :: ExprType }
  
evalExpr :: (Key -> Maybe ExprType) -> Expr -> Either EvalError ExprType
evalExpr get = case _ of
  Val x -> pure x
  If x' y z -> case evalExpr get x' of
    Right (Boolean false) -> evalExpr get z
    Right (Boolean true) -> evalExpr get y
    Right x -> Left (IfCondition x)
    Left x -> Left x
  Equal x' y' -> case evalExpr get x', evalExpr get y' of
    Right (Boolean x), Right (Boolean y) -> Right (Boolean $ x == y)
    Right (Int x), Right (Int y) -> Right (Boolean $ x == y)
    Right (String x), Right (String y) -> Right (Boolean $ x == y)
    Right left, Right right -> Left (EqualMismatch { left, right })
    Left x, _ -> Left x
    _, Left x -> Left x
  Lookup x -> maybe (Left $ MissingKey x) Right (get x)

boolean_ :: Boolean -> Expr
boolean_ = Val <<< Boolean

int_ :: Int -> Expr
int_ = Val <<< Int

string_ :: String -> Expr
string_ = Val <<< String

if_ :: Expr -> Expr -> Expr -> Expr
if_ = If

equal_ :: Expr -> Expr -> Expr
equal_ = Equal

lookup_ :: Key -> Expr
lookup_ = Lookup
