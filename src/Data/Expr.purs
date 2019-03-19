module Lynx.Data.Expr where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, stringify, (.:), (:=), (~>))
import Data.BigInt as Data.BigInt
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe')
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (class Traversable, traverse)
import Matryoshka (anaM, cata, embed, project)
import Ocelot.Data.Currency (Cents, formatCentsToStrDollars)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, Size, arrayOf, oneOf, sized)

type Key = String

data ExprTypeF a
  = Array (Array a)
  | Boolean Boolean
  | Cents Cents
  | Int Int
  | Pair { name :: a, value :: a }
  | String String

derive instance eqExprTypeF :: (Eq a) => Eq (ExprTypeF a)

derive instance eq1ExprTypeF :: Eq1 ExprTypeF

derive instance functorExprTypeF :: Functor ExprTypeF

derive instance genericExprTypeF :: Generic (ExprTypeF a) _

instance showExprTypeF :: (Show a) => Show (ExprTypeF a) where
  show x = genericShow x

instance encodeJsonExprTypeF :: EncodeJson (ExprTypeF (Mu ExprTypeF)) where
  encodeJson = encodeExprType <<< embed

instance decodeJsonExprTypeF :: DecodeJson (ExprTypeF (Mu ExprTypeF)) where
  decodeJson = map project <<< decodeExprType

instance arbitraryExprTypeF :: Arbitrary (ExprTypeF (Mu ExprTypeF)) where
  arbitrary = sized go
    where
      cents :: Gen Cents
      cents = do
        x <- arbitrary
        pure (wrap $ Data.BigInt.fromInt x)
      go :: Size -> Gen (ExprTypeF (Mu ExprTypeF))
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
             (Array <$> (map <<< map) embed (arrayOf $ go size))
             [ Pair <$> do
               name <- map embed (go size)
               value <- map embed (go size)
               pure { name, value }
             ]

instance foldableExprTypeF :: Foldable ExprTypeF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Array x -> foldMap f x
    Boolean _ -> mempty
    Cents _ -> mempty
    Int _ -> mempty
    Pair x -> f x.name <> f x.value
    String _ -> mempty

instance traversableExprTypeF :: Traversable ExprTypeF where
  sequence = traverse identity
  traverse f = case _ of
    Array x -> map Array (traverse f x)
    Boolean x -> pure (Boolean x)
    Cents x -> pure (Cents x)
    Int x -> pure (Int x)
    Pair x -> ado
      name <- f x.name
      value <- f x.value
      in Pair { name, value }
    String x -> pure (String x)

type ExprType
  = Mu ExprTypeF

type ExprTypeJSON
  = { in :: String
    , op :: String
    , out :: String
    , param :: Json
    }

decodeExprType :: Json -> Either String ExprType
decodeExprType x = anaM decodeExprTypeF =<< decodeJson x

decodeExprTypeF :: ExprTypeJSON -> Either String (ExprTypeF ExprTypeJSON)
decodeExprTypeF json@{ out, param } = case out of
  "Array" -> map Array (decodeJson param)
  "Boolean" -> map Boolean (decodeJson param)
  "Cents" -> map (Cents <<< wrap <<< Data.BigInt.fromInt) (decodeJson param)
  "Int" -> map Int (decodeJson param)
  "Pair" -> map Pair (decodeJson param)
  "String" -> map String (decodeJson param)
  _ ->
    Left
      ( show json { param = stringify param }
        <> " unsupported."
        <> " Expected Array, Boolean, Cents, Int, Pair, or String."
      )

encodeExprType :: ExprType -> Json
encodeExprType x = encodeJson (cata encodeExprTypeF x)

encodeExprTypeF :: ExprTypeF ExprTypeJSON -> ExprTypeJSON
encodeExprTypeF = case _ of
  Array x -> base "Array" (encodeJson x)
  Boolean x -> base "Boolean" (encodeJson x)
  Cents x -> base "Cents" (encodeJson x)
  Int x -> base "Int" (encodeJson x)
  Pair x -> base "Pair" (encodeJson x)
  String x -> base "String" (encodeJson x)
  where
  base out param = { in: "Void", op: "Val", out, param }

reflectType :: ExprType -> String
reflectType x = (cata encodeExprTypeF x).out

print :: ExprType -> String
print = cata printF

printF :: ExprTypeF String -> String
printF = case _ of
  Array x -> show x
  Boolean x -> show x
  Cents x -> formatCentsToStrDollars x
  Int x -> show x
  Pair x -> show x
  String x -> x

toArray :: ExprType -> Maybe (Array ExprType)
toArray = toArrayF <<< project

toArrayF :: forall a. ExprTypeF a -> Maybe (Array a)
toArrayF = case _ of
  Array x -> Just x
  otherwise -> Nothing

toBoolean :: ExprType -> Maybe Boolean
toBoolean = cata case _ of
  Boolean x -> Just x
  otherwise -> Nothing

toCents :: ExprType -> Maybe Cents
toCents = cata case _ of
  Cents x -> Just x
  otherwise -> Nothing

toInt :: ExprType -> Maybe Int
toInt = cata case _ of
  Int x     -> Just x
  otherwise -> Nothing

toPair :: ExprType -> Maybe { name :: ExprType, value :: ExprType }
toPair = toPairF <<< project

toPairF :: forall a. ExprTypeF a -> Maybe { name :: a, value :: a }
toPairF = case _ of
  Pair x -> Just x
  otherwise -> Nothing

toString :: ExprType -> Maybe String
toString = cata case _ of
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
      "Val" -> map Val (decodeExprType json)
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
        map (Val <<< embed) arbitrary
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
      ValA x -> encodeExprType x
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
    case project x of
      Boolean false -> evalExpr get z
      Boolean true -> evalExpr get y
      _ -> Left (IfCondition x)
  Equal x' y' -> do
    left <- evalExpr get x'
    right <- evalExpr get y'
    case project left, project right of
      Boolean x, Boolean y -> Right (embed $ Boolean $ x == y)
      Int x, Int y -> Right (embed $ Boolean $ x == y)
      String x, String y -> Right (embed $ Boolean $ x == y)
      _, _ -> Left (EqualMismatch { left, right })
  Print x' -> map (embed <<< String <<< print) (evalExpr get x')
  Lookup x y -> maybe' (\_ -> evalExpr get y) Right (get x)

array_ :: Array ExprType -> ExprType
array_ = embed <<< Array

boolean_ :: Boolean -> Expr
boolean_ = Val <<< embed <<< Boolean

cents_ :: Cents -> Expr
cents_ = Val <<< embed <<< Cents

int_ :: Int -> Expr
int_ = Val <<< embed <<< Int

pair_ :: { name :: ExprType, value :: ExprType } -> ExprType
pair_ = embed <<< Pair

string_ :: String -> Expr
string_ = Val <<< embed <<< String

if_ :: Expr -> Expr -> Expr -> Expr
if_ = If

equal_ :: Expr -> Expr -> Expr
equal_ = Equal

print_ :: Expr -> Expr
print_ = Print

lookup_ :: Key -> Expr -> Expr
lookup_ = Lookup
