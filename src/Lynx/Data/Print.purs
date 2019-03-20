module Lynx.Data.Print where

import Prelude

import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Functor.Coproduct.Inject (class Inject, inj)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Traversable (class Traversable, traverse)
import Lynx.Data.ExprType (ExprJSON, ExprType, print, string_)
import Matryoshka (class Corecursive, embed)

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

decodePrintF :: ExprJSON -> Either String (PrintF ExprJSON)
decodePrintF json@{ op, params } = case op of
  "Print" -> traverse decodeJson params >>= case _ of
    [x] -> pure (Print x)
    _ -> Left "Expected 1 params"
  _ -> Left (op <> " invalid op")

encodePrintF :: PrintF ExprJSON -> ExprJSON
encodePrintF = case _ of
  Print x@{ in } ->
    { in, op: "Print", out: "String", params: map encodeJson [x] }

evalPrintF :: PrintF ExprType -> ExprType
evalPrintF = case _ of
  Print x -> string_ (print x)

print_ :: forall expr f. Corecursive expr f => Inject PrintF f => expr -> expr
print_ = embed <<< inj <<< Print
