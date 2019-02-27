module Lynx.Data.Expr where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Either (Either(..))
import Data.Leibniz (type (~), coerceSymm)
import Data.Maybe (Maybe(..))
import Type.Prelude (Proxy(..))

type Key = String

class HasLookup f o where
  lookup :: Key -> f -> Maybe o

data ExprType
  = Boolean Boolean
  | Int Int
  | String String

reflectType :: ExprType -> String
reflectType (Boolean _) = "Boolean"
reflectType (Int _) = "Int"
reflectType (String _) = "String"

class (Show a, Eq a) <= Expressible a where
  toExprType :: a -> ExprType
  reflectProxy :: Proxy a -> String
  print :: a -> String

instance exprBoolean :: Expressible Boolean where
  toExprType = Boolean
  reflectProxy _ = "Boolean"
  print = show

instance exprInt :: Expressible Int where
  toExprType = Int
  reflectProxy _ = "Int"
  print = show

instance exprString :: Expressible String where
  toExprType = String
  reflectProxy _ = "String"
  print = identity

instance exprVoid :: Expressible Void where
  toExprType = absurd
  reflectProxy _ = "Void"
  print = show

data ExprF i o
  = Val (i ~ Void) o
  | Lookup (i ~ Void) Key (Expr o) (∀ f. HasLookup f o => f -> Maybe o)
  | If (i ~ Boolean) (Expr Boolean) (Expr o) (Expr o)
  | Equal (o ~ Boolean) (Expr i) (Expr i)
  | Print (o ~ String) (Expr i)

data Expr o = Expr (∀ e. (∀ i. Expressible i => ExprF i o -> e) -> e)

data ExprA
  = ValA ExprType
  | IfA ExprA ExprA ExprA String String
  | EqualA ExprA ExprA String String
  | PrintA ExprA String String
  | LookupA Key ExprA String

mkExpr
  :: ∀ i o
   . Expressible i
  => Expressible o
  => ExprF i o
  -> Expr o
mkExpr e = Expr (_ $ e)

val_ :: forall o. Expressible o => o -> Expr o
val_ x = mkExpr (Val identity x)

if_
  :: forall o
   . Expressible o
  => Expr Boolean
  -> Expr o
  -> Expr o
  -> Expr o
if_ x y z = mkExpr (If identity x y z)

equal_
  :: forall o
   . Expressible o
  => Expr o
  -> Expr o
  -> Expr Boolean
equal_ x y = mkExpr (Equal identity x y)

print_
  :: forall o
   . Expressible o
  => Expr o
  -> Expr String
print_ x = mkExpr (Print identity x)

lookup_
  :: forall o
   . Expressible o
  => Key
  -> Expr o
  -> (∀ f. f -> Maybe o)
  -> Expr o
lookup_ k x f = mkExpr (Lookup identity k x f)

runExpr
  :: ∀ o e
   . (∀ i. Expressible i => ExprF i o -> e)
  -> Expr o
  -> e
runExpr r (Expr f) = f r

evalExpr'
  :: ∀ f o
   . HasLookup f o
  => HasLookup f Boolean
  => Expressible o
  => f
  -> Expr o
  -> o
evalExpr' f = runExpr eval
  where
  eval :: ∀ i. HasLookup f i => Expressible i => ExprF i o -> o
  eval (Val _ x) = x
  eval (If p x y z) = if evalExpr' f x then evalExpr' f y else evalExpr' f z
  eval (Equal p x y) = coerceSymm p $ evalExpr' f x == evalExpr' f y
  eval (Print p x) = coerceSymm p $ print $ evalExpr' f x
  eval (Lookup _ _ x get) = case get f of
    Nothing -> evalExpr' f x
    Just y -> y

toExprA :: ∀ o. Expressible o => Expr o -> ExprA
toExprA = runExpr go
  where
  go :: ∀ i. Expressible i => ExprF i o -> ExprA
  go (Val _ x) = ValA (toExprType x)
  go (If _ x y z) = IfA (toExprA x) (toExprA y) (toExprA z)
    (reflectProxy $ Proxy :: Proxy i)
    (reflectProxy $ Proxy :: Proxy o)
  go (Equal _ x y) = EqualA (toExprA x) (toExprA y)
    (reflectProxy $ Proxy :: Proxy i)
    (reflectProxy $ Proxy :: Proxy o)
  go (Print _ x) = PrintA (toExprA x)
    (reflectProxy $ Proxy :: Proxy i)
    (reflectProxy $ Proxy :: Proxy o)
  go (Lookup _ k x get) = LookupA k (toExprA x)
    (reflectProxy $ Proxy :: Proxy o)

instance showExpr :: Expressible o => Show (Expr o) where
  show = runExpr go
    where
      go :: ∀ i. Expressible i => ExprF i o -> String
      go (Val _ x) = "(Val _ " <> show x <> ")"
      go (If _ x y z) = "(If _ " <> show x <> " " <> show y <> " " <> show z <> ")"
      go (Equal _ x y) = "(Equal _ " <> show x <> " " <> show y <> ")"
      go (Print _ x) = "(Print _ " <> show x <> ")"
      go (Lookup _ k x _) = "(Lookup _ " <> show k <> " " <> show x <> " _)"

instance encodeExprType :: EncodeJson ExprType where
  encodeJson (Boolean x) = encodeJson x
  encodeJson (Int x) = encodeJson x
  encodeJson (String x) = encodeJson x

instance encodeExprA :: EncodeJson ExprA where
  encodeJson (ValA x) =
    "param" := x ~> encodeHelper "Val" "Void" (reflectType x)
  encodeJson (IfA x y z i o) =
    "params" := [ x, y, z ] ~> encodeHelper "If" i o
  encodeJson (EqualA x y i o) =
    "params" := [ x, y ] ~> encodeHelper "Equal" i o
  encodeJson (PrintA x i o) =
    "params" := [ x ] ~> encodeHelper "Print" i o
  encodeJson (LookupA k x o) =
    "params" := [ encodeJson k, encodeJson x ] ~> encodeHelper "Lookup" "Void" o

instance encodeExprBoolean :: EncodeJson (Expr Boolean) where
  encodeJson = encodeJson <<< toExprA
else instance encodeExprInt :: EncodeJson (Expr Int) where
  encodeJson = encodeJson <<< toExprA
else instance encodeExprString :: EncodeJson (Expr String) where
  encodeJson = encodeJson <<< toExprA

encodeHelper :: String -> String -> String -> Json
encodeHelper op i o =
  "op" := op ~> "in" := i ~> "out" := o ~> jsonEmptyObject

instance decodeExprBoolean :: DecodeJson (Expr Boolean) where
  decodeJson json = do
    x <- decodeJson json
    x .: "in" >>= case _ of
      "Boolean" -> do
        e :: ExprF Boolean Boolean <- decodeJson json
        pure $ mkExpr e
      "Int" -> do
        e :: ExprF Int Boolean <- decodeJson json
        pure $ mkExpr e
      "String" -> do
        e :: ExprF String Boolean <- decodeJson json
        pure $ mkExpr e
      "Void" -> do
        e :: ExprF Void Boolean <- decodeJson json
        pure $ mkExpr e
      other -> Left $ other <> " not implemented"
else instance decodeExprInt :: DecodeJson (Expr Int) where
  decodeJson json = do
    x <- decodeJson json
    x .: "in" >>= case _ of
      "Boolean" -> do
        e :: ExprF Boolean Int <- decodeJson json
        pure $ mkExpr e
      "Int" -> do
        e :: ExprF Int Int <- decodeJson json
        pure $ mkExpr e
      "String" -> do
        e :: ExprF String Int <- decodeJson json
        pure $ mkExpr e
      "Void" -> do
        e :: ExprF Void Int <- decodeJson json
        pure $ mkExpr e
      other -> Left $ other <> " not implemented"
else instance decodeExprString :: DecodeJson (Expr String) where
  decodeJson json = do
    x <- decodeJson json
    x .: "in" >>= case _ of
      "Boolean" -> do
        e :: ExprF Boolean String <- decodeJson json
        pure $ mkExpr e
      "Int" -> do
        e :: ExprF Int String <- decodeJson json
        pure $ mkExpr e
      "String" -> do
        e :: ExprF String String <- decodeJson json
        pure $ mkExpr e
      "Void" -> do
        e :: ExprF Void String <- decodeJson json
        pure $ mkExpr e
      other -> Left $ other <> " not implemented"

instance decodeVal :: (DecodeJson o, DecodeJson (Expr o)) => DecodeJson (ExprF Void o) where
  decodeJson json = do
    x <- decodeJson json
    (x .: "param" >>= pure <<< Val identity) <|> (x .: "params" >>= decodeLookupF)
else instance decodeExprFBooleanBoolean :: DecodeJson (ExprF Boolean Boolean) where
  decodeJson json = do
    x <- decodeJson json
    params <- x .: "params"
    x .: "op" >>= case _ of
      "If" -> decodeIfF params
      "Equal" -> decodeEqualF params
      op -> Left $ op <> " invald op"
else instance decodeExprFIBoolean :: DecodeJson (Expr i) => DecodeJson (ExprF i Boolean) where
  decodeJson json = do
    x <- decodeJson json
    params <- x .: "params"
    x .: "op" >>= case _ of
      "Equal" -> decodeEqualF params
      op -> Left $ op <> " invalid op"
else instance decodeExprFBooleanO :: DecodeJson (Expr o) => DecodeJson (ExprF Boolean o) where
  decodeJson json = do
    x <- decodeJson json
    params <- x .: "params"
    x .: "op" >>= case _ of
      "If" -> decodeIfF params
      op -> Left $ op <> " invalid op"
else instance decodeExprFOther :: DecodeJson (ExprF i o) where
  decodeJson _ = Left "Unimplemented"

decodeLookupF
  :: ∀ o
   . DecodeJson (Expr o)
  => Array Json
  -> Either String (ExprF Void o)
decodeLookupF [ k, x ] = do
  k' :: Key <- decodeJson k
  x' :: Expr o <- decodeJson x
  pure $ Lookup identity k' x' (lookup k')
decodeLookupF xs = Left "Expected 2 params"

decodeIfF
  :: ∀ o
   . DecodeJson (Expr o)
  => Array Json
  -> Either String (ExprF Boolean o)
decodeIfF [ x, y, z ] = do
  x' :: Expr Boolean <- decodeJson x
  y' :: Expr o <- decodeJson y
  z' :: Expr o <- decodeJson z
  pure $ If identity x' y' z'
decodeIfF xs = Left "Expceted 3 params"

decodeEqualF
  :: ∀ i
   . DecodeJson (Expr i)
  => Array Json
  -> Either String (ExprF i Boolean)
decodeEqualF [ x, y ] = do
  x' :: Expr i <- decodeJson x
  y' :: Expr i <- decodeJson y
  pure $ Equal identity x' y'
decodeEqualF xs = Left "Expected 2 params"
