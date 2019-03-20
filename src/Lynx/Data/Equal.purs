module Lynx.Data.Equal where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Functor.Coproduct.Inject (class Inject, inj)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse)
import Lynx.Data.ExprType (ExprJSON, ExprType, boolean_, reflectType, toBoolean, toInt, toString)
import Matryoshka (class Corecursive, embed)

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

decodeEqualF :: ExprJSON -> Either String (EqualF ExprJSON)
decodeEqualF json@{ op, params } = case op of
  "Equal" -> traverse decodeJson params >>= case _ of
    [x, y] -> pure (Equal x y)
    _ -> Left "Expected 2 params"
  _ -> Left (op <> " invalid op")

encodeEqualF :: EqualF ExprJSON -> ExprJSON
encodeEqualF = case _ of
  Equal x@{ in } y ->
    { in, op: "Equal", out: "Boolean", params: map encodeJson [x, y] }

data EvalError
  = Mismatch { left :: ExprType, right :: ExprType }

renderEvalError :: EvalError -> String
renderEvalError = case _ of
  Mismatch x ->
    "Expected both sides of equal to have the same type,"
      <> " but they are different."
      <> " left: "
      <> reflectType x.left
      <> " right: "
      <> reflectType x.right

evalEqualF :: EqualF ExprType -> Either EvalError ExprType
evalEqualF = case _ of
  Equal left right -> case result left right of
    Just x -> pure (boolean_ x)
    Nothing -> Left (Mismatch { left, right })
  where
  boolean left right = do
    x <- toBoolean left
    y <- toBoolean right
    pure (x == y)
  int left right = do
    x <- toInt left
    y <- toInt right
    pure (x == y)
  result left right =
    boolean left right
      <|> int left right
      <|> string left right
  string left right = do
    x <- toString left
    y <- toString right
    pure (x == y)

equal_ ::
  forall expr f.
  Corecursive expr f =>
  Inject EqualF f =>
  expr ->
  expr ->
  expr
equal_ x = embed <<< inj <<< Equal x
