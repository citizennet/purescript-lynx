module Lynx.Data.ExprType where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, stringify)
import Data.BigInt as Data.BigInt
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
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

-- | The inductive version of `ExprTypeF _`.
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
        (embed <<< Boolean <$> arbitrary)
        [ embed <<< Cents <$> cents
        , embed <<< Int <$> arbitrary
        , embed <<< String <$> arbitrary
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

instance corecursiveExprTypeExprTypeF :: Corecursive ExprType ExprTypeF where
  embed x = ExprType (embed $ map (un ExprType) x)

instance decodeJsonExprType :: DecodeJson ExprType where
  decodeJson = decodeExprType

instance encodeJsonExprType :: EncodeJson ExprType where
  encodeJson = encodeExprType

instance recursiveExprTypeExprTypeF :: Recursive ExprType ExprTypeF where
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
decodeExprTypeF json@{ out, params } = case out of
  "Array" -> map Array (traverse decodeJson params)
  "Boolean" -> traverse decodeJson params >>= case _ of
    [x] -> pure (Boolean x)
    _ -> Left "Expected 1 param"
  "Cents" -> traverse decodeJson params >>= case _ of
    [x] -> pure (Cents $ wrap $ Data.BigInt.fromInt x)
    _ -> Left "Expected 1 param"
  "Int" -> traverse decodeJson params >>= case _ of
    [x] -> pure (Int x)
    _ -> Left "Expected 1 param"
  "Pair" -> traverse decodeJson params >>= case _ of
    [x] -> pure (Pair x)
    _ -> Left "Expected 1 param"
  "String" -> traverse decodeJson params >>= case _ of
    [x] -> pure (String x)
    _ -> Left "Expected 1 param"
  _ ->
    Left
      ( show json { params = map stringify params }
        <> " unsupported."
        <> " Expected Array, Boolean, Cents, Int, Pair, or String."
      )

encodeExprType :: ExprType -> Json
encodeExprType x = encodeJson (cata encodeExprTypeF x)

encodeExprTypeF :: ExprTypeF ExprJSON -> ExprJSON
encodeExprTypeF = case _ of
  Array x -> base "Array" x
  Boolean x -> base "Boolean" [x]
  Cents x -> base "Cents" [x]
  Int x -> base "Int" [x]
  Pair x -> base "Pair" [x]
  String x -> base "String" [x]
  where
  base :: forall a. EncodeJson a => String -> Array a -> ExprJSON
  base out params = { in: "Void", op: "Val", out, params: map encodeJson params }

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

array_ :: Array ExprType -> ExprType
array_ = embed <<< Array

boolean_ :: Boolean -> ExprType
boolean_ = embed <<< Boolean

cents_ :: Cents -> ExprType
cents_ = embed <<< Cents

int_ :: Int -> ExprType
int_ = embed <<< Int

pair_ :: { name :: ExprType, value :: ExprType } -> ExprType
pair_ = embed <<< Pair

string_ :: String -> ExprType
string_ = embed <<< String
