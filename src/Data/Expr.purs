module Lynx.Data.Expr where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
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

instance encodeJsonExprType :: EncodeJson ExprType where
  encodeJson = case _ of
    Boolean x -> encodeJson x
    Int x -> encodeJson x
    String x -> encodeJson x

reflectType :: ExprType -> String
reflectType (Boolean _) = "Boolean"
reflectType (Int _) = "Int"
reflectType (String _) = "String"

print :: ExprType -> ExprType
print = case _ of
  Boolean x -> String (show x)
  Int x -> String (show x)
  String x -> String x

data Expr
  = Val ExprType
  | If Expr Expr Expr
  | Equal Expr Expr
  | Print Expr
  | Lookup Key

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
      "Lookup" -> x' .: "param" >>= pure <<< lookup_
      op -> Left $ op <> " invalid op"

data ExprA
  = ValA ExprType
  | IfA ExprA ExprA ExprA String String
  | EqualA ExprA ExprA String String
  | PrintA ExprA String String
  | LookupA Key String

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
      LookupA x o -> "param" := x ~> encodeHelper "Lookup" "Void" o

toExprA :: Expr -> ExprA
toExprA = case _ of
  Val x -> ValA x
  If x y z -> IfA (toExprA x) (toExprA y) (toExprA z) (reflectIn x) (reflectOut y)
  Equal x y -> EqualA (toExprA x) (toExprA y) (reflectIn x) (reflectOut x)
  Print x -> PrintA (toExprA x) (reflectIn x) "String"
  Lookup x -> LookupA x "???"
  where
  reflectIn :: Expr -> String
  reflectIn = case _ of
    Val _ -> "Void"
    If x _ _ -> reflectIn x
    Equal x _ -> reflectIn x
    Print x -> reflectIn x
    Lookup _ -> "Void"
  reflectOut :: Expr -> String
  reflectOut = case _ of
    Val x -> reflectType x
    If _ x _ -> reflectOut x
    Equal _ _ -> "Boolean"
    Print x -> "String"
    Lookup _ -> "???"

encodeHelper :: String -> String -> String -> Json
encodeHelper op i o =
  "op" := op ~> "in" := i ~> "out" := o ~> jsonEmptyObject

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
  Print x' -> case evalExpr get x' of
    Right x -> Right (print x)
    Left x -> Left x
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
