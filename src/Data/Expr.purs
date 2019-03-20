module Lynx.Data.Expr where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Either.Inject as Data.Either.Inject
import Data.Either.Nested (type (\/))
import Data.Functor.Coproduct.Inject (inj)
import Data.Functor.Coproduct.Nested (type (<\/>), (<\/>))
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, un)
import Data.NonEmpty (NonEmpty(..))
import Lynx.Data.Equal (EqualF, decodeEqualF, encodeEqualF, equal_, evalEqualF)
import Lynx.Data.Equal as Lynx.Data.Equal
import Lynx.Data.ExprType (ExprType)
import Lynx.Data.ExprType as Lynx.Data.ExprType
import Lynx.Data.If (IfF, decodeIfF, encodeIfF, evalIfF, if_)
import Lynx.Data.If as Lynx.Data.If
import Lynx.Data.Lookup (Key, LookupF, decodeLookupF, encodeLookupF, evalLookupF, lookup_)
import Lynx.Data.Print (PrintF, decodePrintF, encodePrintF, evalPrintF, print_)
import Lynx.Data.Val (ValF, decodeValF, encodeValF, evalValF, val_)
import Matryoshka (class Corecursive, class Recursive, anaM, cata, cataM, embed, project)
import Ocelot.Data.Currency (Cents)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, Size, oneOf, sized)

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

decodeExpr :: Json -> Either String Expr
decodeExpr x' = anaM go =<< decodeJson x'
  where
  go x =
    map inj (decodeValF x)
      <|> map inj (decodeIfF x)
      <|> map inj (decodeEqualF x)
      <|> map inj (decodePrintF x)
      <|> map inj (decodeLookupF x)

encodeExpr :: Expr -> Json
encodeExpr x = encodeJson (cata go x)
  where
  go =
    encodeValF
      <\/> encodeIfF
      <\/> encodeEqualF
      <\/> encodePrintF
      <\/> encodeLookupF

type EvalError
  = Lynx.Data.If.EvalError
  \/ Lynx.Data.Equal.EvalError

renderEvalError :: EvalError -> String
renderEvalError =
  Lynx.Data.If.renderEvalError
    `either` Lynx.Data.Equal.renderEvalError

evalExpr :: (Key -> Maybe ExprType) -> Expr -> Either EvalError ExprType
evalExpr get = cataM go
  where
  go =
    pure <<< evalValF
      <\/> lmap Data.Either.Inject.inj <<< evalIfF
      <\/> lmap Data.Either.Inject.inj <<< evalEqualF
      <\/> pure <<< evalPrintF
      <\/> pure <<< evalLookupF get

boolean_ :: Boolean -> Expr
boolean_ = val_ <<< Lynx.Data.ExprType.boolean_

cents_ :: Cents -> Expr
cents_ = val_ <<< Lynx.Data.ExprType.cents_

int_ :: Int -> Expr
int_ = val_ <<< Lynx.Data.ExprType.int_

string_ :: String -> Expr
string_ = val_ <<< Lynx.Data.ExprType.string_
