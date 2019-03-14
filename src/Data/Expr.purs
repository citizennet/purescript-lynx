module Lynx.Data.Expr where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, stringify, (.:), (:=), (~>))
import Data.BigInt as Data.BigInt
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe')
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty(..))
import Ocelot.Data.Currency (Cents, formatCentsToStrDollars)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, Size, arrayOf, oneOf, sized)

type Key = String

data ExprType
  = Array (Array ExprType)
  | Boolean Boolean
  | Cents Cents
  | Int Int
  | Pair { name :: ExprType, value :: ExprType }
  | String String

derive instance eqExprType :: Eq ExprType

derive instance genericExprType :: Generic ExprType _

instance showExprType :: Show ExprType where
  show x = genericShow x

instance encodeJsonExprType :: EncodeJson ExprType where
  encodeJson x' = case x' of
    Array x -> "param" := encodeJson x ~> base x'
    Boolean x -> "param" := encodeJson x ~> base x'
    Cents x -> "param" := encodeJson x ~> base x'
    Int x -> "param" := encodeJson x ~> base x'
    Pair x -> "param" := encodeJson x ~> base x'
    String x -> "param" := encodeJson x ~> base x'
    where
    base :: ExprType -> Json
    base x =
      "in" := encodeJson "Void"
        ~> "out" := encodeJson (reflectType x)
        ~> "op" := encodeJson "Val"
        ~> jsonEmptyObject

instance decodeJsonExprType :: DecodeJson ExprType where
  decodeJson json' = do
    json <- decodeJson json'
    out <- json .: "out"
    param <- json .: "param"
    case out, param of
      "Array", x -> map Array (decodeJson x)
      "Boolean", x -> map Boolean (decodeJson x)
      "Cents", x -> map Cents (cents x)
      "Int", x -> map Int (decodeJson x)
      "Pair", x -> map Pair (decodeJson x)
      "String", x -> map String (decodeJson x)
      _, _ ->
        Left
          ( stringify json'
            <> " unsupported."
            <> " Expected Array, Boolean, Cents, Int, Pair, or String."
          )
    where
    cents :: Json -> Either String Cents
    cents json = do
      x <- decodeJson json
      pure (wrap $ Data.BigInt.fromInt x)

instance arbitraryExprType :: Arbitrary ExprType where
  arbitrary = sized go
    where
      cents :: Gen Cents
      cents = do
        x <- arbitrary
        pure (wrap $ Data.BigInt.fromInt x)
      go :: Size -> Gen ExprType
      go size' =
        if size' < 1 then
          oneOf $ NonEmpty
            (Boolean <$> arbitrary)
            [ Cents <$> cents
            , Int <$> arbitrary
            , String <$> arbitrary
            ]
        else
          let size = size' / 10
          in oneOf $ NonEmpty
             (Array <$> arrayOf (go size))
             [ Pair <$> ({ name: _, value: _ } <$> go size <*> go size)
             ]

reflectType :: ExprType -> String
reflectType (Array _) = "Array"
reflectType (Boolean _) = "Boolean"
reflectType (Cents _) = "Cents"
reflectType (Int _) = "Int"
reflectType (Pair _) = "Pair"
reflectType (String _) = "String"

print :: ExprType -> String
print = case _ of
  Array x -> "[" <> intercalate ", " (map print x) <> "]"
  Boolean x -> show x
  Cents x -> formatCentsToStrDollars x
  Int x -> show x
  Pair x -> "{name: " <> print x.name <> ", value: " <> print x.value <> "}"
  String x -> x

toArray :: ExprType -> Maybe (Array ExprType)
toArray = case _ of
  Array x -> Just x
  otherwise -> Nothing

toBoolean :: ExprType -> Maybe Boolean
toBoolean = case _ of
  Boolean x -> Just x
  otherwise -> Nothing

toCents :: ExprType -> Maybe Cents
toCents = case _ of
  Cents x -> Just x
  otherwise -> Nothing

toInt :: ExprType -> Maybe Int
toInt = case _ of
  Int x     -> Just x
  otherwise -> Nothing

toPair :: ExprType -> Maybe { name :: ExprType, value :: ExprType }
toPair = case _ of
  Pair x -> Just x
  otherwise -> Nothing

toString :: ExprType -> Maybe String
toString = case _ of
  String x  -> Just x
  otherwise -> Nothing

data Expr
  = Val ExprType
  | If Expr Expr Expr
  | Equal Expr Expr
  | Print Expr
  | Lookup Key Expr

derive instance eqExpr :: Eq Expr

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

instance encodeJsonExpr :: EncodeJson Expr where
  encodeJson = encodeJson <<< toExprA

instance decodeJsonExpr :: DecodeJson Expr where
  decodeJson json = do
    x' <- decodeJson json
    x' .: "op" >>= case _ of
      "Val" -> map Val (decodeJson json)
      "If" -> x' .: "params" >>= case _ of
        [x, y, z] -> pure (if_ x y z)
        _ -> Left "Expected 3 params"
      "Equal" -> x' .: "params" >>= case _ of
        [x, y] -> pure (equal_ x y)
        _ -> Left "Expected 2 params"
      "Print" -> x' .: "params" >>= case _ of
        [x] -> pure (print_ x)
        _ -> Left "Expected 1 param"
      "Lookup" -> x' .: "params" >>= case _ of
        [x, y] -> do
          key <- decodeJson x
          default <- decodeJson y
          pure (lookup_ key default)
        _ -> Left "Expected 2 params"
      op -> Left $ op <> " invalid op"

instance arbitraryExpr :: Arbitrary Expr where
  arbitrary = sized go
    where
    go :: Size -> Gen Expr
    go size' =
      if size' < 1 then
        map Val arbitrary
      else
        let size = size' / 10
        in oneOf $ NonEmpty
            (If <$> go size <*> go size <*> go size)
            [ Equal <$> go size <*> go size
            , Print <$> go size
            , Lookup <$> arbitrary <*> go size
            ]

data ExprA
  = ValA ExprType
  | IfA ExprA ExprA ExprA String String
  | EqualA ExprA ExprA String String
  | PrintA ExprA String String
  | LookupA Key ExprA String

instance encodeJsonExprA :: EncodeJson ExprA where
  encodeJson =
    case _ of
      ValA x -> encodeJson x
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

derive instance eqEvalError :: Eq EvalError

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

cents_ :: Cents -> Expr
cents_ = Val <<< Cents

int_ :: Int -> Expr
int_ = Val <<< Int

string_ :: String -> Expr
string_ = Val <<< String

if_ :: Expr -> Expr -> Expr -> Expr
if_ = If

equal_ :: Expr -> Expr -> Expr
equal_ = Equal

print_ :: Expr -> Expr
print_ = Print

lookup_ :: Key -> Expr -> Expr
lookup_ = Lookup
