module Lynx.Data.Lookup where

import Prelude

import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Functor.Coproduct.Inject (class Inject, inj)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (class Traversable, traverse)
import Lynx.Data.ExprType (ExprType, ExprJSON)
import Matryoshka (class Corecursive, embed)

type Key = String

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

decodeLookupF :: ExprJSON -> Either String (LookupF ExprJSON)
decodeLookupF json@{ op, params } = case op of
  "Lookup" -> case params of
    [x', y'] -> do
      x <- decodeJson x'
      y <- decodeJson y'
      pure (Lookup x y)
    _ -> Left "Expected 2 params"
  _ -> Left (op <> " invalid op")

encodeLookupF :: LookupF ExprJSON -> ExprJSON
encodeLookupF = case _ of
  Lookup x y@{ out } ->
    { in: "Void", op: "Lookup", out, params: [encodeJson x, encodeJson y] }

evalLookupF :: (Key -> Maybe ExprType) -> LookupF ExprType -> ExprType
evalLookupF get = case _ of
  Lookup x y -> fromMaybe y (get x)

lookup_ ::
  forall expr f.
  Corecursive expr f =>
  Inject LookupF f =>
  Key ->
  expr ->
  expr
lookup_ x = embed <<< inj <<< Lookup x
