module Lynx.Data.ExprType where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, stringify)
import Data.BigInt as Data.BigInt
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Coproduct.Inject (inj, prj)
import Data.Functor.Coproduct.Nested (type (<\/>), (<\/>))
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, un, wrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (class Traversable, traverse)
import Matryoshka (class Corecursive, class Recursive, anaM, cata, embed, project)
import Ocelot.Data.Currency (Cents, formatCentsToStrDollars)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, Size, arrayOf, oneOf, sized)

-- | Our non-recursive type of base expressions.
-- | These are the underlying values that get rendered in the form.
-- | Though other things may build upon it,
-- | ultimately we can only render these values.
data ArrayF a
  = Array (Array a)

derive instance eqArrayF :: (Eq a) => Eq (ArrayF a)

derive instance eq1ArrayF :: Eq1 ArrayF

derive instance functorArrayF :: Functor ArrayF

derive instance genericArrayF :: Generic (ArrayF a) _

instance showArrayF :: (Show a) => Show (ArrayF a) where
  show x = genericShow x

instance foldableArrayF :: Foldable ArrayF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Array x -> foldMap f x

instance traversableArrayF :: Traversable ArrayF where
  sequence = traverse identity
  traverse f = case _ of
    Array x -> map Array (traverse f x)

data BooleanF a
  = Boolean Boolean

derive instance eqBooleanF :: (Eq a) => Eq (BooleanF a)

derive instance eq1BooleanF :: Eq1 BooleanF

derive instance functorBooleanF :: Functor BooleanF

derive instance genericBooleanF :: Generic (BooleanF a) _

instance showBooleanF :: (Show a) => Show (BooleanF a) where
  show x = genericShow x

instance foldableBooleanF :: Foldable BooleanF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Boolean _ -> mempty

instance traversableBooleanF :: Traversable BooleanF where
  sequence = traverse identity
  traverse f = case _ of
    Boolean x -> pure (Boolean x)

data CentsF a
  = Cents Cents

derive instance eqCentsF :: (Eq a) => Eq (CentsF a)

derive instance eq1CentsF :: Eq1 CentsF

derive instance functorCentsF :: Functor CentsF

derive instance genericCentsF :: Generic (CentsF a) _

instance showCentsF :: (Show a) => Show (CentsF a) where
  show x = genericShow x

instance foldableCentsF :: Foldable CentsF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Cents _ -> mempty

instance traversableCentsF :: Traversable CentsF where
  sequence = traverse identity
  traverse f = case _ of
    Cents x -> pure (Cents x)

data IntF a
  = Int Int

derive instance eqIntF :: (Eq a) => Eq (IntF a)

derive instance eq1IntF :: Eq1 IntF

derive instance functorIntF :: Functor IntF

derive instance genericIntF :: Generic (IntF a) _

instance showIntF :: (Show a) => Show (IntF a) where
  show x = genericShow x

instance foldableIntF :: Foldable IntF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Int _ -> mempty

instance traversableIntF :: Traversable IntF where
  sequence = traverse identity
  traverse f = case _ of
    Int x -> pure (Int x)

data PairF a
  = Pair { name :: a, value :: a }

derive instance eqPairF :: (Eq a) => Eq (PairF a)

derive instance eq1PairF :: Eq1 PairF

derive instance functorPairF :: Functor PairF

derive instance genericPairF :: Generic (PairF a) _

instance showPairF :: (Show a) => Show (PairF a) where
  show x = genericShow x

instance foldablePairF :: Foldable PairF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Pair x -> f x.name <> f x.value

instance traversablePairF :: Traversable PairF where
  sequence = traverse identity
  traverse f = case _ of
    Pair x -> ado
      name <- f x.name
      value <- f x.value
      in Pair { name, value }

data StringF a
  = String String

derive instance eqStringF :: (Eq a) => Eq (StringF a)

derive instance eq1StringF :: Eq1 StringF

derive instance functorStringF :: Functor StringF

derive instance genericStringF :: Generic (StringF a) _

instance showStringF :: (Show a) => Show (StringF a) where
  show x = genericShow x

instance foldableStringF :: Foldable StringF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    String _ -> mempty

instance traversableStringF :: Traversable StringF where
  sequence = traverse identity
  traverse f = case _ of
    String x -> pure (String x)

type ExprTypeF
  = ArrayF <\/> BooleanF <\/> CentsF <\/> IntF <\/> PairF <\/> StringF

-- | The inductive version of our base expressions.
-- | This is what most of our consumers should be dealing with.
newtype ExprType
  = ExprType (Mu ExprTypeF)

derive instance newtypeExprType :: Newtype ExprType _

derive newtype instance eqExprType :: Eq ExprType

derive newtype instance showExprType :: Show ExprType

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
            [ pair_ <$> do
              name <- go size
              value <- go size
              pure { name, value }
            ]

instance corecursiveExprTypeExprTypeF :: Corecursive ExprType (ArrayF <\/> BooleanF <\/> CentsF <\/> IntF <\/> PairF <\/> StringF) where
  embed x = ExprType (embed $ map (un ExprType) x)

instance decodeJsonExprType :: DecodeJson ExprType where
  decodeJson = decodeExprType

instance encodeJsonExprType :: EncodeJson ExprType where
  encodeJson = encodeExprType

instance recursiveExprTypeExprTypeF :: Recursive ExprType (ArrayF <\/> BooleanF <\/> CentsF <\/> IntF <\/> PairF <\/> StringF) where
  project (ExprType x) = map ExprType (project x)

-- | The JSON representation of `Expr`.
-- | We create this type so we know what we're decoding/encoding into.
-- | It's also useful for the morphisms we write.
type ExprJSON
  = { in :: String
    , op :: String
    , out :: String
    , params :: Array Json
    }

decodeExprType :: Json -> Either String ExprType
decodeExprType x = anaM decodeExprTypeF =<< decodeJson x

decodeExprTypeF :: ExprJSON -> Either String (ExprTypeF ExprJSON)
decodeExprTypeF x =
  map inj (decodeArrayF x)
    <|> map inj (decodeBooleanF x)
    <|> map inj (decodeCentsF x)
    <|> map inj (decodeIntF x)
    <|> map inj (decodePairF x)
    <|> map inj (decodeStringF x)
    <|> Left
      ( show x { params = map stringify x.params }
        <> " unsupported."
        <> " Expected Array, Boolean, Cents, Int, Pair, or String."
      )

decodeArrayF :: ExprJSON -> Either String (ArrayF ExprJSON)
decodeArrayF json@{ out, params } = case out of
  "Array" -> map Array (traverse decodeJson params)
  _ -> Left (show json { params = map stringify params } <> " unsupported.")

decodeBooleanF :: ExprJSON -> Either String (BooleanF ExprJSON)
decodeBooleanF json@{ out, params } = case out of
  "Boolean" -> traverse decodeJson params >>= case _ of
    [x] -> pure (Boolean x)
    _ -> Left "Expected 1 param"
  _ -> Left (show json { params = map stringify params } <> " unsupported.")

decodeCentsF :: ExprJSON -> Either String (CentsF ExprJSON)
decodeCentsF json@{ out, params } = case out of
  "Cents" -> traverse decodeJson params >>= case _ of
    [x] -> pure (Cents $ wrap $ Data.BigInt.fromInt x)
    _ -> Left "Expected 1 param"
  _ -> Left (show json { params = map stringify params } <> " unsupported.")

decodeIntF :: ExprJSON -> Either String (IntF ExprJSON)
decodeIntF json@{ out, params } = case out of
  "Int" -> traverse decodeJson params >>= case _ of
    [x] -> pure (Int x)
    _ -> Left "Expected 1 param"
  _ -> Left (show json { params = map stringify params } <> " unsupported.")

decodePairF :: ExprJSON -> Either String (PairF ExprJSON)
decodePairF json@{ out, params } = case out of
  "Pair" -> traverse decodeJson params >>= case _ of
    [x] -> pure (Pair x)
    _ -> Left "Expected 1 param"
  _ -> Left (show json { params = map stringify params } <> " unsupported.")

decodeStringF :: ExprJSON -> Either String (StringF ExprJSON)
decodeStringF json@{ out, params } = case out of
  "String" -> traverse decodeJson params >>= case _ of
    [x] -> pure (String x)
    _ -> Left "Expected 1 param"
  _ -> Left (show json { params = map stringify params } <> " unsupported.")

encodeExprType :: ExprType -> Json
encodeExprType x = encodeJson (cata encodeExprTypeF x)

encodeExprTypeF :: ExprTypeF ExprJSON -> ExprJSON
encodeExprTypeF =
    encodeArrayF
      <\/> encodeBooleanF
      <\/> encodeCentsF
      <\/> encodeIntF
      <\/> encodePairF
      <\/> encodeStringF

encodeArrayF :: ArrayF ExprJSON -> ExprJSON
encodeArrayF = case _ of
  Array x -> base "Array" x

encodeBooleanF :: BooleanF ExprJSON -> ExprJSON
encodeBooleanF = case _ of
  Boolean x -> base "Boolean" [x]

encodeCentsF :: CentsF ExprJSON -> ExprJSON
encodeCentsF = case _ of
  Cents x -> base "Cents" [x]

encodeIntF :: IntF ExprJSON -> ExprJSON
encodeIntF = case _ of
  Int x -> base "Int" [x]

encodePairF :: PairF ExprJSON -> ExprJSON
encodePairF = case _ of
  Pair x -> base "Pair" [x]

encodeStringF :: StringF ExprJSON -> ExprJSON
encodeStringF = case _ of
  String x -> base "String" [x]

base :: forall a. EncodeJson a => String -> Array a -> ExprJSON
base out params = { in: "Void", op: "Val", out, params: map encodeJson params }

reflectType :: ExprType -> String
reflectType x = (cata encodeExprTypeF x).out

print :: ExprType -> String
print = cata printF

printF :: ExprTypeF String -> String
printF =
  printArrayF
    <\/> printBooleanF
    <\/> printCentsF
    <\/> printIntF
    <\/> printPairF
    <\/> printStringF

printArrayF :: ArrayF String -> String
printArrayF = case _ of
  Array x -> show x

printBooleanF :: BooleanF String -> String
printBooleanF = case _ of
  Boolean x -> show x

printCentsF :: CentsF String -> String
printCentsF = case _ of
  Cents x -> formatCentsToStrDollars x

printIntF :: IntF String -> String
printIntF = case _ of
  Int x -> show x

printPairF :: PairF String -> String
printPairF = case _ of
  Pair x -> show x

printStringF :: StringF String -> String
printStringF = case _ of
  String x -> x

toArray :: ExprType -> Maybe (Array ExprType)
toArray x' = do
  Array x <- prj (project x')
  pure x

toBoolean :: ExprType -> Maybe Boolean
toBoolean x' = do
  Boolean x <- prj (project x')
  pure x

toCents :: ExprType -> Maybe Cents
toCents x' = do
  Cents x <- prj (project x')
  pure x

toInt :: ExprType -> Maybe Int
toInt x' = do
  Int x <- prj (project x')
  pure x

toPair :: ExprType -> Maybe { name :: ExprType, value :: ExprType }
toPair x' = do
  Pair x <- prj (project x')
  pure x

toString :: ExprType -> Maybe String
toString x' = do
  String x <- prj (project x')
  pure x

array_ :: Array ExprType -> ExprType
array_ = embed <<< inj <<< Array

boolean_ :: Boolean -> ExprType
boolean_ = embed <<< inj <<< Boolean

cents_ :: Cents -> ExprType
cents_ = embed <<< inj <<< Cents

int_ :: Int -> ExprType
int_ = embed <<< inj <<< Int

pair_ :: { name :: ExprType, value :: ExprType } -> ExprType
pair_ = embed <<< inj <<< Pair

string_ :: String -> ExprType
string_ = embed <<< inj <<< String
