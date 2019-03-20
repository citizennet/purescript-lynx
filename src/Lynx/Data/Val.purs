module Lynx.Data.Val where

import Prelude

import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Functor.Coproduct.Inject (class Inject, inj)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Traversable (class Traversable, traverse)
import Lynx.Data.ExprType (ExprType, ExprJSON, decodeExprTypeF, encodeExprTypeF)
import Matryoshka (class Corecursive, anaM, cata, embed)

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

decodeValF :: ExprJSON -> Either String (ValF ExprJSON)
decodeValF json@{ op, params } = case op of
  "Val" -> map Val (anaM decodeExprTypeF json)
  _ -> Left (op <> " invalid op")

encodeValF :: ValF ExprJSON -> ExprJSON
encodeValF = case _ of
  Val x -> cata encodeExprTypeF x

evalValF :: ValF ExprType -> ExprType
evalValF = case _ of
  Val x -> x

val_ :: forall expr f. Corecursive expr f => Inject ValF f => ExprType -> expr
val_ = embed <<< inj <<< Val
