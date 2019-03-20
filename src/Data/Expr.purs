module Lynx.Data.Expr where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, stringify)
import Data.BigInt as Data.BigInt
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Coproduct.Inject (inj)
import Data.Functor.Coproduct.Nested (type (<\/>), (<\/>))
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un, wrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (class Traversable, traverse)
import Matryoshka (class Corecursive, class Recursive, anaM, cata, cataM, embed, project)
import Ocelot.Data.Currency (Cents, formatCentsToStrDollars)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, Size, arrayOf, oneOf, sized)

type Key = String

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

-- | An expression representing a basic value.
-- | There's no recursion here, so when we have one of these,
-- | we know when can do something directly with it.
data ValF a
  = Val ExprType

derive instance eqValF :: (Eq a) => Eq (ValF a)

derive instance eq1ValF :: Eq1 ValF

derive instance functorValF :: Functor ValF

derive instance genericValF :: Generic (ValF a) _

instance showValF :: (Show a) => Show (ValF a) where
  show x = genericShow x

instance foldableValF :: Foldable ValF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Val _ -> mempty

instance traversableValF :: Traversable ValF where
  sequence = traverse identity
  traverse f = case _ of
    Val x -> pure (Val x)

-- | An expression for conditionally choosing another expression.
-- | When used recursively, it will contain the "condition,"
-- | the "true" expression, and the "false" expression.
data IfF a
  = If a a a

derive instance genericIfF :: Generic (IfF a) _

derive instance eqIfF :: (Eq a) => Eq (IfF a)

derive instance eq1IfF :: Eq1 IfF

derive instance functorIfF :: Functor IfF

instance showIfF :: (Show a) => Show (IfF a) where
  show = genericShow

instance foldableIfF :: Foldable IfF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    If x y z -> f x <> f y <> f z

instance traversableIfF :: Traversable IfF where
  sequence = traverse identity
  traverse f = case _ of
    If x' y' z' -> ado
      x <- f x'
      y <- f y'
      z <- f z'
      in If x y z

-- | An expression for checking if two expressions are equal.
-- | When used recursively,
-- | it will contain the two expressions to check for equality.
data EqualF a
  = Equal a a

derive instance genericEqualF :: Generic (EqualF a) _

derive instance eqEqualF :: (Eq a) => Eq (EqualF a)

derive instance eq1EqualF :: Eq1 EqualF

derive instance functorEqualF :: Functor EqualF

instance showEqualF :: (Show a) => Show (EqualF a) where
  show = genericShow

instance foldableEqualF :: Foldable EqualF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Equal x y -> f x <> f y

instance traversableEqualF :: Traversable EqualF where
  sequence = traverse identity
  traverse f = case _ of
    Equal x' y' -> ado
      x <- f x'
      y <- f y'
      in Equal x y

-- | An expression for printing out an expression.
-- | When used recursively, it will contain the expression to print.
data PrintF a
  = Print a

derive instance genericPrintF :: Generic (PrintF a) _

derive instance eqPrintF :: (Eq a) => Eq (PrintF a)

derive instance eq1PrintF :: Eq1 PrintF

derive instance functorPrintF :: Functor PrintF

instance showPrintF :: (Show a) => Show (PrintF a) where
  show = genericShow

instance foldablePrintF :: Foldable PrintF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Print x -> f x

instance traversablePrintF :: Traversable PrintF where
  sequence = traverse identity
  traverse f = case _ of
    Print x -> map Print (f x)

-- | An expression for looking up a key.
-- | When used recursively,
-- | it will contain the default value to use in case the `Key` doesn't exist.
data LookupF a
  = Lookup Key a

derive instance genericLookupF :: Generic (LookupF a) _

derive instance eqLookupF :: (Eq a) => Eq (LookupF a)

derive instance eq1LookupF :: Eq1 LookupF

derive instance functorLookupF :: Functor LookupF

instance showLookupF :: (Show a) => Show (LookupF a) where
  show = genericShow

instance foldableLookupF :: Foldable LookupF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Lookup _ x -> f x

instance traversableLookupF :: Traversable LookupF where
  sequence = traverse identity
  traverse f = case _ of
    Lookup x y -> map (Lookup x) (f y)

-- | The inductive version of expressions.
-- | This is what most of our consumers should be dealing with.
newtype Expr
  = Expr (Mu (ValF <\/> IfF <\/> EqualF <\/> PrintF <\/> LookupF))

derive instance genericExpr :: Generic Expr _

derive instance newtypeExpr :: Newtype Expr _

derive newtype instance eqExpr :: Eq Expr

derive newtype instance showExpr :: Show Expr

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
            (if_ <$> go size <*> go size <*> go size)
            [ equal_ <$> go size <*> go size
            , print_ <$> go size
            , lookup_ <$> arbitrary <*> go size
            ]

instance corecursiveExprValF :: Corecursive Expr (ValF <\/> IfF <\/> EqualF <\/> PrintF <\/> LookupF) where
  embed x = Expr (embed $ map (un Expr) x)

instance decodeJsonExpr :: DecodeJson Expr where
  decodeJson = decodeExpr

instance encodeJsonExpr :: EncodeJson Expr where
  encodeJson = encodeExpr

instance recursiveExprValF :: Recursive Expr (ValF <\/> IfF <\/> EqualF <\/> PrintF <\/> LookupF) where
  project (Expr x) = map Expr (project x)

-- | The JSON representation of `Expr`.
-- | We create this type so we know what we're decoding/encoding into.
-- | It's also useful for the morphisms we write.
type ExprJSON
  = { in :: String
    , op :: String
    , out :: String
    , params :: Array Json
    }

decodeExpr :: Json -> Either String Expr
decodeExpr x' = anaM go =<< decodeJson x'
  where
  go x =
    map inj (decodeValF x)
      <|> map inj (decodeIfF x)
      <|> map inj (decodeEqualF x)
      <|> map inj (decodePrintF x)
      <|> map inj (decodeLookupF x)

decodeValF :: ExprJSON -> Either String (ValF ExprJSON)
decodeValF json@{ op, params } = case op of
  "Val" -> map Val (anaM decodeExprTypeF json)
  _ -> Left (op <> " invalid op")

decodeIfF :: ExprJSON -> Either String (IfF ExprJSON)
decodeIfF json@{ op, params } = case op of
  "If" -> traverse decodeJson params >>= case _ of
    [x, y, z] -> pure (If x y z)
    _ -> Left "Expected 3 params"
  _ -> Left (op <> " invalid op")

decodeEqualF :: ExprJSON -> Either String (EqualF ExprJSON)
decodeEqualF json@{ op, params } = case op of
  "Equal" -> traverse decodeJson params >>= case _ of
    [x, y] -> pure (Equal x y)
    _ -> Left "Expected 2 params"
  _ -> Left (op <> " invalid op")

decodePrintF :: ExprJSON -> Either String (PrintF ExprJSON)
decodePrintF json@{ op, params } = case op of
  "Print" -> traverse decodeJson params >>= case _ of
    [x] -> pure (Print x)
    _ -> Left "Expected 1 params"
  _ -> Left (op <> " invalid op")

decodeLookupF :: ExprJSON -> Either String (LookupF ExprJSON)
decodeLookupF json@{ op, params } = case op of
  "Lookup" -> case params of
    [x', y'] -> do
      x <- decodeJson x'
      y <- decodeJson y'
      pure (Lookup x y)
    _ -> Left "Expected 2 params"
  _ -> Left (op <> " invalid op")

encodeExpr :: Expr -> Json
encodeExpr x = encodeJson (cata go x)
  where
  go =
    encodeValF
      <\/> encodeIfF
      <\/> encodeEqualF
      <\/> encodePrintF
      <\/> encodeLookupF

encodeValF :: ValF ExprJSON -> ExprJSON
encodeValF = case _ of
  Val x -> cata encodeExprTypeF x

encodeIfF :: IfF ExprJSON -> ExprJSON
encodeIfF = case _ of
  If x@{ in } y@{ out } z ->
    { in, op: "If", out, params: map encodeJson [x, y, z] }

encodeEqualF :: EqualF ExprJSON -> ExprJSON
encodeEqualF = case _ of
  Equal x@{ in } y ->
    { in, op: "Equal", out: "Boolean", params: map encodeJson [x, y] }

encodePrintF :: PrintF ExprJSON -> ExprJSON
encodePrintF = case _ of
  Print x@{ in } ->
    { in, op: "Print", out: "String", params: map encodeJson [x] }

encodeLookupF :: LookupF ExprJSON -> ExprJSON
encodeLookupF = case _ of
  Lookup x y@{ out } ->
    { in: "Void", op: "Lookup", out, params: [encodeJson x, encodeJson y] }

data EvalError
  = IfCondition ExprType
  | EqualMismatch { left :: ExprType, right :: ExprType }

derive instance eqEvalError :: Eq EvalError

evalExpr :: (Key -> Maybe ExprType) -> Expr -> Either EvalError ExprType
evalExpr get = cataM go
  where
  go =
    pure <<< evalValF
      <\/> evalIfF
      <\/> evalEqualF
      <\/> pure <<< evalPrintF
      <\/> pure <<< evalLookupF get

evalValF :: ValF ExprType -> ExprType
evalValF = case _ of
  Val x -> x

evalIfF :: IfF ExprType -> Either EvalError ExprType
evalIfF = case _ of
  If x y z -> do
    case project x of
      Boolean false -> pure z
      Boolean true -> pure y
      _ -> Left (IfCondition x)

evalEqualF :: EqualF ExprType -> Either EvalError ExprType
evalEqualF = case _ of
  Equal left right -> do
    case project left, project right of
      Boolean x, Boolean y -> Right (embed $ Boolean $ x == y)
      Int x, Int y -> Right (embed $ Boolean $ x == y)
      String x, String y -> Right (embed $ Boolean $ x == y)
      _, _ -> Left (EqualMismatch { left, right })

evalPrintF :: PrintF ExprType -> ExprType
evalPrintF = case _ of
  Print x -> embed (String $ print x)

evalLookupF :: (Key -> Maybe ExprType) -> LookupF ExprType -> ExprType
evalLookupF get = case _ of
  Lookup x y -> fromMaybe y (get x)

array_ :: Array ExprType -> ExprType
array_ = embed <<< Array

boolean_ :: Boolean -> Expr
boolean_ = val_ <<< embed <<< Boolean

cents_ :: Cents -> Expr
cents_ = val_ <<< embed <<< Cents

int_ :: Int -> Expr
int_ = val_ <<< embed <<< Int

pair_ :: { name :: ExprType, value :: ExprType } -> ExprType
pair_ = embed <<< Pair

string_ :: String -> Expr
string_ = val_ <<< embed <<< String

if_ :: Expr -> Expr -> Expr -> Expr
if_ x y = embed <<< inj <<< If x y

equal_ :: Expr -> Expr -> Expr
equal_ x = embed <<< inj <<< Equal x

print_ :: Expr -> Expr
print_ = embed <<< inj <<< Print

lookup_ :: Key -> Expr -> Expr
lookup_ x = embed <<< inj <<< Lookup x

val_ :: ExprType -> Expr
val_ = embed <<< inj <<< Val
