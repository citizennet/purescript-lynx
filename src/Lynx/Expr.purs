module Lynx.Expr where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, stringify, (.:), (:=), (~>))
import Data.BigInt as Data.BigInt
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, fold, foldlDefault, foldrDefault)
import Data.Formatter.DateTime (format, unformat)
import Data.Formatter.Parser.Interval (extendedDateTimeFormatInUTC)
import Data.Functor.Coproduct.Inject (inj, prj)
import Data.Functor.Coproduct.Nested (type (<\/>), (<\/>))
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Newtype (class Newtype, un, wrap)
import Data.NonEmpty (NonEmpty(..))
import Data.String.Common (replace)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Traversable (class Traversable, traverse)
import Foreign.Object (Object)
import Foreign.Object as Foreign.Object
import Matryoshka (class Corecursive, class Recursive, anaM, cata, embed, project)
import Ocelot.Data.Currency (Cents, formatCentsToStrDollars)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, Size, arrayOf, oneOf, sized)

type Key = String

newtype ArrayF a
  = Array (Array a)

derive instance eqArrayF :: (Eq a) => Eq (ArrayF a)

derive instance eq1ArrayF :: Eq1 ArrayF

derive instance functorArrayF :: Functor ArrayF

derive instance genericArrayF :: Generic (ArrayF a) _

derive newtype instance foldableArrayF :: Foldable ArrayF

derive newtype instance traversableArrayF :: Traversable ArrayF

instance showArrayF :: (Show a) => Show (ArrayF a) where
  show = genericShow

decodeArrayF :: ExprJSON -> Either String (ArrayF ExprJSON)
decodeArrayF { out, param } = case out of
  "Array" -> map Array (decodeJson param)
  _ -> Left "Expected Array"

encodeArrayF :: ArrayF ExprJSON -> ExprJSON
encodeArrayF = case _ of
  Array x -> base "Array" x

isEmptyArrayF :: forall a. ArrayF a -> Boolean
isEmptyArrayF = case _ of
  Array [] -> true
  Array _ -> false

printArrayF :: ArrayF String -> String
printArrayF = case _ of
  Array x -> show x

newtype BooleanF a
  = Boolean Boolean

derive instance eqBooleanF :: Eq (BooleanF a)

derive instance eq1BooleanF :: Eq1 BooleanF

derive instance functorBooleanF :: Functor BooleanF

derive instance genericBooleanF :: Generic (BooleanF a) _

instance foldableBooleanF :: Foldable BooleanF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Boolean _ -> mempty

instance showBooleanF :: Show (BooleanF a) where
  show = genericShow

instance traversableBooleanF :: Traversable BooleanF where
  sequence = traverse identity
  traverse f = case _ of
    Boolean x -> pure (Boolean x)

decodeBooleanF :: forall a. ExprJSON -> Either String (BooleanF a)
decodeBooleanF { out, param } = case out of
  "Boolean" -> map Boolean (decodeJson param)
  _ -> Left "Expected Boolean"

encodeBooleanF :: forall a. BooleanF a -> ExprJSON
encodeBooleanF = case _ of
  Boolean x -> base "Boolean" x

isEmptyBooleanF :: forall a. BooleanF a -> Boolean
isEmptyBooleanF = case _ of
  Boolean x -> false

printBooleanF :: forall a. BooleanF a -> String
printBooleanF = case _ of
  Boolean x -> show x

newtype CentsF a
  = Cents Cents

derive instance eqCentsF :: Eq (CentsF a)

derive instance eq1CentsF :: Eq1 CentsF

derive instance functorCentsF :: Functor CentsF

derive instance genericCentsF :: Generic (CentsF a) _

instance foldableCentsF :: Foldable CentsF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Cents _ -> mempty

instance showCentsF :: Show (CentsF a) where
  show = genericShow

instance traversableCentsF :: Traversable CentsF where
  sequence = traverse identity
  traverse f = case _ of
    Cents x -> pure (Cents x)

decodeCentsF :: forall a. ExprJSON -> Either String (CentsF a)
decodeCentsF { out, param } = case out of
  "Cents" -> map (Cents <<< wrap <<< Data.BigInt.fromInt) (decodeJson param)
  _ -> Left "Expected Cents"

encodeCentsF :: forall a. CentsF a -> ExprJSON
encodeCentsF = case _ of
  Cents x -> base "Cents" x

isEmptyCentsF :: forall a. CentsF a -> Boolean
isEmptyCentsF = case _ of
  Cents x -> false

printCentsF :: forall a. CentsF a -> String
printCentsF = case _ of
  Cents x -> replace (Pattern ".00") (Replacement "") (formatCentsToStrDollars x)

newtype DateTimeF a
  = DateTime DateTime

derive instance eqDateTimeF :: Eq (DateTimeF a)

derive instance eq1DateTimeF :: Eq1 DateTimeF

derive instance functorDateTimeF :: Functor DateTimeF

derive instance genericDateTimeF :: Generic (DateTimeF a) _

instance foldableDateTimeF :: Foldable DateTimeF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    DateTime _ -> mempty

instance showDateTimeF :: Show (DateTimeF a) where
  show = genericShow

instance traversableDateTimeF :: Traversable DateTimeF where
  sequence = traverse identity
  traverse f = case _ of
    DateTime x -> pure (DateTime x)

decodeDateTimeF :: forall a. ExprJSON -> Either String (DateTimeF a)
decodeDateTimeF { out, param } = case out of
  "DateTime" -> do
    x' <- decodeJson param
    x <- unformat extendedDateTimeFormatInUTC x'
    pure (DateTime x)
  _ -> Left "Expected DateTime"

encodeDateTimeF :: forall a. DateTimeF a -> ExprJSON
encodeDateTimeF = case _ of
  DateTime x -> base "DateTime" (format extendedDateTimeFormatInUTC x)

isEmptyDateTimeF :: forall a. DateTimeF a -> Boolean
isEmptyDateTimeF = case _ of
  DateTime x -> false

printDateTimeF :: forall a. DateTimeF a -> String
printDateTimeF = case _ of
  DateTime x -> format extendedDateTimeFormatInUTC x

newtype IntF a
  = Int Int

derive instance eqIntF :: Eq (IntF a)

derive instance eq1IntF :: Eq1 IntF

derive instance functorIntF :: Functor IntF

derive instance genericIntF :: Generic (IntF a) _

instance foldableIntF :: Foldable IntF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Int _ -> mempty

instance showIntF :: Show (IntF a) where
  show = genericShow

instance traversableIntF :: Traversable IntF where
  sequence = traverse identity
  traverse f = case _ of
    Int x -> pure (Int x)

decodeIntF :: forall a. ExprJSON -> Either String (IntF a)
decodeIntF { out, param } = case out of
  "Int" -> map Int (decodeJson param)
  _ -> Left "Expected Int"

encodeIntF :: forall a. IntF a -> ExprJSON
encodeIntF = case _ of
  Int x -> base "Int" x

isEmptyIntF :: forall a. IntF a -> Boolean
isEmptyIntF = case _ of
  Int x -> false

printIntF :: forall a. IntF a -> String
printIntF = case _ of
  Int x -> show x

newtype PairF a
  = Pair { name :: a, value :: a }

derive instance eqPairF :: (Eq a) => Eq (PairF a)

derive instance eq1PairF :: Eq1 PairF

derive instance functorPairF :: Functor PairF

derive instance genericPairF :: Generic (PairF a) _

instance foldablePairF :: Foldable PairF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Pair x -> f x.name <> f x.value

instance showPairF :: (Show a) => Show (PairF a) where
  show = genericShow

instance traversablePairF :: Traversable PairF where
  sequence = traverse identity
  traverse f = case _ of
    Pair x -> ado
      name <- f x.name
      value <- f x.value
      in Pair { name, value }

decodePairF :: ExprJSON -> Either String (PairF ExprJSON)
decodePairF { out, param } = case out of
  "Pair" -> map Pair (decodeJson param)
  _ -> Left "Expected Pair"

encodePairF :: PairF ExprJSON -> ExprJSON
encodePairF = case _ of
  Pair x -> base "Pair" x

isEmptyPairF :: forall a. PairF a -> Boolean
isEmptyPairF = case _ of
  Pair x -> false

printPairF :: PairF String -> String
printPairF = case _ of
  Pair x -> show x

newtype StringF a
  = String String

derive instance eqStringF :: Eq (StringF a)

derive instance eq1StringF :: Eq1 StringF

derive instance functorStringF :: Functor StringF

derive instance genericStringF :: Generic (StringF a) _

instance foldableStringF :: Foldable StringF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    String _ -> mempty

instance showStringF :: Show (StringF a) where
  show = genericShow

instance traversableStringF :: Traversable StringF where
  sequence = traverse identity
  traverse f = case _ of
    String x -> pure (String x)

decodeStringF :: forall a. ExprJSON -> Either String (StringF a)
decodeStringF { out, param } = case out of
  "String" -> map String (decodeJson param)
  _ -> Left "Expected String"

encodeStringF :: forall a. StringF a -> ExprJSON
encodeStringF = case _ of
  String x -> base "String" x

isEmptyStringF :: forall a. StringF a -> Boolean
isEmptyStringF = case _ of
  String "" -> true
  String _ -> false

printStringF :: forall a. StringF a -> String
printStringF = case _ of
  String x -> x

type ExprJSON
  = { in :: String
    , out :: String
    , op :: String
    , param :: Json
    }

base :: forall a. EncodeJson a => String -> a -> ExprJSON
base out param = { in: "Void", out, op: "Val", param: encodeJson param }

type ExprTypeF
  = ArrayF
    <\/> BooleanF
    <\/> CentsF
    <\/> DateTimeF
    <\/> IntF
    <\/> PairF
    <\/> StringF

decodeExprTypeF :: ExprJSON -> Either String (ExprTypeF ExprJSON)
decodeExprTypeF x =
  map inj (decodeArrayF x)
    <|> map inj (decodeBooleanF x)
    <|> map inj (decodeCentsF x)
    <|> map inj (decodeDateTimeF x)
    <|> map inj (decodeIntF x)
    <|> map inj (decodePairF x)
    <|> map inj (decodeStringF x)
    <|> Left
      ( show x { param = stringify x.param }
        <> " unsupported."
        <> " Expected Array, Boolean, Cents, DateTime, Int, Pair, or String."
      )

encodeExprTypeF :: ExprTypeF ExprJSON -> ExprJSON
encodeExprTypeF =
  encodeArrayF
    <\/> encodeBooleanF
    <\/> encodeCentsF
    <\/> encodeDateTimeF
    <\/> encodeIntF
    <\/> encodePairF
    <\/> encodeStringF

isEmptyExprTypeF :: forall a. ExprTypeF a -> Boolean
isEmptyExprTypeF =
  isEmptyArrayF
    <\/> isEmptyBooleanF
    <\/> isEmptyCentsF
    <\/> isEmptyDateTimeF
    <\/> isEmptyIntF
    <\/> isEmptyPairF
    <\/> isEmptyStringF

printExprTypeF :: ExprTypeF String -> String
printExprTypeF =
  printArrayF
    <\/> printBooleanF
    <\/> printCentsF
    <\/> printDateTimeF
    <\/> printIntF
    <\/> printPairF
    <\/> printStringF

newtype ExprType
  = ExprType (Mu ExprTypeF)

derive instance eqExprType :: Eq ExprType

derive instance genericExprType :: Generic ExprType _

derive instance newtypeExprType :: Newtype ExprType _

instance encodeJsonExprType :: EncodeJson ExprType where
  encodeJson x = encodeJson (cata encodeExprTypeF x)

instance decodeJsonExprType :: DecodeJson ExprType where
  decodeJson json = anaM decodeExprTypeF =<< decodeJson json

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
            (boolean_ <$> arbitrary)
            [ cents_ <$> cents
            , int_ <$> arbitrary
            , string_ <$> arbitrary
            ]
        else
          let size = size' / 10
          in oneOf $ NonEmpty
             (array_ <$> arrayOf (go size))
             [ pair_ <$> ({ name: _, value: _ } <$> go size <*> go size)
             ]

instance corecursiveExprTypeExprTypeF ::
  Corecursive
    ExprType
    ( ArrayF
      <\/> BooleanF
      <\/> CentsF
      <\/> DateTimeF
      <\/> IntF
      <\/> PairF
      <\/> StringF
    )
    where
      embed x = ExprType (embed $ map (un ExprType) x)

instance recursiveExprTypeExprTypeF ::
  Recursive
    ExprType
    ( ArrayF
      <\/> BooleanF
      <\/> CentsF
      <\/> DateTimeF
      <\/> IntF
      <\/> PairF
      <\/> StringF
    )
    where
      project (ExprType x) = map ExprType (project x)

instance showExprType :: Show ExprType where
  show x = genericShow x

isEmpty :: Maybe ExprType -> Boolean
isEmpty = maybe true (cata isEmptyExprTypeF)

reflectType :: ExprType -> String
reflectType x = (cata encodeExprTypeF x).out

print :: ExprType -> String
print = cata printExprTypeF

toArray :: ExprType -> Maybe (Array ExprType)
toArray x' = do
  Array x <- prj (project x')
  pure x

toBoolean :: ExprType -> Maybe Boolean
toBoolean x' = do
  Boolean x <- prj (project x')
  pure x

toDateTime :: ExprType -> Maybe DateTime
toDateTime x' = do
  DateTime x <- prj (project x')
  pure x

toCents :: ExprType -> Maybe Cents
toCents x' = do
  Cents x <- prj (project x')
  pure x

toInt :: ExprType -> Maybe Int
toInt x' = do
  Int x <- prj (project x')
  pure x

toObject :: ExprType -> Object String
toObject item = fold do
  { name: name', value: value' } <- toPair item
  name <- toString name'
  value <- toString value'
  pure (Foreign.Object.fromHomogeneous { name, value })

toPair :: ExprType -> Maybe { name :: ExprType, value :: ExprType }
toPair x' = do
  Pair x <- prj (project x')
  pure x

toString :: ExprType -> Maybe String
toString x' = do
  String x <- prj (project x')
  pure x

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
      "Val" -> map val_ (decodeJson json)
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
        map val_ arbitrary
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
    case toBoolean x of
      Just false -> evalExpr get z
      Just true -> evalExpr get y
      _ -> Left (IfCondition x)
  Equal x' y' -> do
    left <- evalExpr get x'
    right <- evalExpr get y'
    case toBoolean left, toBoolean right of
      Just x, Just y -> Right (boolean_ $ x == y)
      _, _ -> case toInt left, toInt right of
        Just x, Just y -> Right (boolean_ $ x == y)
        _, _ -> case toString left, toString right of
          Just x, Just y -> Right (boolean_ $ x == y)
          _, _ -> Left (EqualMismatch { left, right })
  Print x' -> map (string_ <<< print) (evalExpr get x')
  Lookup x y -> maybe' (\_ -> evalExpr get y) Right (get x)

array_ :: Array ExprType -> ExprType
array_ x = embed (inj $ Array x)

boolean_ :: Boolean -> ExprType
boolean_ x = embed (inj $ Boolean x)

cents_ :: Cents -> ExprType
cents_ x = embed (inj $ Cents x)

datetime_ :: DateTime -> ExprType
datetime_ x = embed (inj $ DateTime x)

int_ :: Int -> ExprType
int_ x = embed (inj $ Int x)

pair_ :: { name :: ExprType, value :: ExprType } -> ExprType
pair_ x = embed (inj $ Pair x)

string_ :: String -> ExprType
string_ x = embed (inj $ String x)

if_ :: Expr -> Expr -> Expr -> Expr
if_ = If

equal_ :: Expr -> Expr -> Expr
equal_ = Equal

print_ :: Expr -> Expr
print_ = Print

lookup_ :: Key -> Expr -> Expr
lookup_ = Lookup

val_ :: ExprType -> Expr
val_ = Val
