module Lynx.Data.Expr where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Functor.Coproduct.Inject (inj)
import Data.Functor.Coproduct.Nested (type (<\/>), (<\/>))
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (class Traversable, traverse)
import Lynx.Data.ExprType (ExprType, ExprTypeF(..), decodeExprTypeF, encodeExprTypeF, print)
import Lynx.Data.ExprType as Lynx.Data.ExprType
import Matryoshka (class Corecursive, class Recursive, anaM, cata, cataM, embed, project)
import Ocelot.Data.Currency (Cents)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, Size, oneOf, sized)

type Key = String

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
  Print x -> Lynx.Data.ExprType.string_ $ print x

evalLookupF :: (Key -> Maybe ExprType) -> LookupF ExprType -> ExprType
evalLookupF get = case _ of
  Lookup x y -> fromMaybe y (get x)

boolean_ :: Boolean -> Expr
boolean_ = val_ <<< Lynx.Data.ExprType.boolean_

cents_ :: Cents -> Expr
cents_ = val_ <<< Lynx.Data.ExprType.cents_

int_ :: Int -> Expr
int_ = val_ <<< Lynx.Data.ExprType.int_

string_ :: String -> Expr
string_ = val_ <<< Lynx.Data.ExprType.string_

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
