module Lynx.Data.Expr where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, maybe')

type Key = String

data ExprType
  = Boolean Boolean
  | Int Int
  | String String

derive instance genericExprType :: Generic ExprType _

instance showExprType :: Show ExprType where
  show = genericShow

instance encodeJsonExprType :: EncodeJson ExprType where
  encodeJson = case _ of
    Boolean x -> encodeJson x
    Int x -> encodeJson x
    String x -> encodeJson x

reflectType :: ExprType -> String
reflectType (Boolean _) = "Boolean"
reflectType (Int _) = "Int"
reflectType (String _) = "String"

print :: ExprType -> String
print = case _ of
  Boolean x -> show x
  Int x -> show x
  String x -> x

data Expr
  = Val ExprType
  | If Expr Expr Expr
  | Equal Expr Expr
  | Print Expr
  | Lookup Key Expr

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

instance encodeJsonExpr :: EncodeJson Expr where
  encodeJson = encodeJson <<< toExprA

instance decodeJsonExpr :: DecodeJson Expr where
  decodeJson json = do
    x' <- decodeJson json
    x' .: "op" >>= case _ of
      "Val" -> x' .: "out" >>= case _ of
        "Boolean" -> x' .: "param" >>= pure <<< boolean_
        "Int" -> x' .: "param" >>= pure <<< int_
        "String" -> x' .: "param" >>= pure <<< string_
        out -> Left $ out <> " not implemented"
      "If" -> x' .: "params" >>= case _ of
        [x, y, z] -> pure (if_ x y z)
        _ -> Left "Expected 3 params"
      "Equal" -> x' .: "params" >>= case _ of
        [x, y] -> pure (equal_ x y)
        _ -> Left "Expected 2 params"
      "Lookup" -> x' .: "params" >>= case _ of
        [x, y] -> do
          key <- decodeJson x
          default <- decodeJson y
          pure (lookup_ key default)
        _ -> Left "Expected 2 params"
      op -> Left $ op <> " invalid op"

data ExprA
  = ValA ExprType
  | IfA ExprA ExprA ExprA String String
  | EqualA ExprA ExprA String String
  | PrintA ExprA String String
  | LookupA Key ExprA String

instance encodeJsonExprA :: EncodeJson ExprA where
  encodeJson =
    case _ of
      ValA x -> "param" := x ~> encodeHelper "Val" "Void" (reflectType x)
      IfA x y z i o ->
        "params" := [ x, y, z ] ~> encodeHelper "If" i o
      EqualA x y i o ->
        "params" := [ x, y ] ~> encodeHelper "Equal" i o
      PrintA x i o ->
        "params" := [ x ] ~> encodeHelper "Print" i o
      LookupA x y o ->
        "params" := [ encodeJson x, encodeJson y ] ~> encodeHelper "Lookup" "Void" o

toExprA :: Expr -> ExprA
toExprA = case _ of
  Val x -> ValA x
  If x y z -> IfA (toExprA x) (toExprA y) (toExprA z) (reflectIn x) (reflectOut y)
  Equal x y -> EqualA (toExprA x) (toExprA y) (reflectIn x) (reflectOut x)
  Print x -> PrintA (toExprA x) (reflectIn x) "String"
  Lookup x y -> LookupA x (toExprA y) (reflectOut y)
  where
  reflectIn :: Expr -> String
  reflectIn = case _ of
    Val _ -> "Void"
    If x _ _ -> reflectIn x
    Equal x _ -> reflectIn x
    Print x -> reflectIn x
    Lookup _ _ -> "Void"
  reflectOut :: Expr -> String
  reflectOut = case _ of
    Val x -> reflectType x
    If _ x _ -> reflectOut x
    Equal _ _ -> "Boolean"
    Print x -> "String"
    Lookup _ x -> reflectOut x

encodeHelper :: String -> String -> String -> Json
encodeHelper op i o =
  "op" := op ~> "in" := i ~> "out" := o ~> jsonEmptyObject

data EvalError
  = IfCondition ExprType
  | EqualMismatch { left :: ExprType, right :: ExprType }

evalExpr :: (Key -> Maybe ExprType) -> Expr -> Either EvalError ExprType
evalExpr get = case _ of
  Val x -> pure x
  If x' y z -> do
    x <- evalExpr get x'
    case x of
      Boolean false -> evalExpr get z
      Boolean true -> evalExpr get y
      _ -> Left (IfCondition x)
  Equal x' y' -> do
    left <- evalExpr get x'
    right <- evalExpr get y'
    case left, right of
      Boolean x, Boolean y -> Right (Boolean $ x == y)
      Int x, Int y -> Right (Boolean $ x == y)
      String x, String y -> Right (Boolean $ x == y)
      _, _ -> Left (EqualMismatch { left, right })
  Print x' -> map (String <<< print) (evalExpr get x')
  Lookup x y -> maybe' (\_ -> evalExpr get y) Right (get x)

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

lookup_ :: Key -> Expr -> Expr
lookup_ = Lookup
