module Lynx.Data.If where

import Prelude

import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Functor.Coproduct.Inject (class Inject, inj)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Traversable (class Traversable, traverse)
import Lynx.Data.ExprType (ExprJSON, ExprType, ExprTypeF(..), reflectType)
import Matryoshka (class Corecursive, embed, project)

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

decodeIfF :: ExprJSON -> Either String (IfF ExprJSON)
decodeIfF json@{ op, params } = case op of
  "If" -> traverse decodeJson params >>= case _ of
    [x, y, z] -> pure (If x y z)
    _ -> Left "Expected 3 params"
  _ -> Left (op <> " invalid op")

encodeIfF :: IfF ExprJSON -> ExprJSON
encodeIfF = case _ of
  If x@{ in } y@{ out } z ->
    { in, op: "If", out, params: map encodeJson [x, y, z] }

data EvalError
  = Condition ExprType

renderEvalError :: EvalError -> String
renderEvalError = case _ of
  Condition x ->
    "Expected conditional to be a Boolean, but its type is: " <> reflectType x

evalIfF :: IfF ExprType -> Either EvalError ExprType
evalIfF = case _ of
  If x y z -> do
    case project x of
      Boolean false -> pure z
      Boolean true -> pure y
      _ -> Left (Condition x)

if_ ::
  forall expr f.
  Corecursive expr f =>
  Inject IfF f =>
  expr ->
  expr ->
  expr ->
  expr
if_ x y = embed <<< inj <<< If x y
