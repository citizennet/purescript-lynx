module Lynx.List
  ( Column
  , View
  , fromJSON
  , toJSON
  ) where

import Prelude
import Data.Argonaut as Data.Argonaut
import Data.Codec.Argonaut as Data.Codec.Argonaut
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Lynx.Expr as Lynx.Expr

-- | The intent of providing `JSONView` as an abstraction instead of using doing
-- | our serialization directly on `View` is that it decouples the
-- | implementation of `View` from the serialization of the concept of a "view."
-- | Hopefully, this should keep downstream consumers from having to change
-- | in lockstep with our codebase as our ideas and understanding evolve
-- | (E.g. renaming fields in our type).
-- |
-- | While it might look almost the exact same as the `View` right now,
-- | we provide `JSONView` a separate entity so we always suppor the decoupling.
type JSONView
  = { columns :: Array JSONColumn
    }

type JSONColumn
  = { name :: Lynx.Expr.Expr
    , value :: Lynx.Expr.Key
    }

-- | The respresentation of a "view" that we really want to work with.
-- | As our understanding of what we're implementing grows,
-- | we will want to make changes to `View`
-- | rather directly in the serialization (`JSONView`).
-- |
-- | This is provided so we don't have to be careful about our serialization.
-- | We should not fear changing anything about `View`,
-- | so long as it's isomorphic to `JSONView`.
type View
  = { columns :: Array Column
    }

type Column
  = { name :: Lynx.Expr.Expr
    , value :: Lynx.Expr.Key
    }

fromArgonaut ::
  forall a.
  Data.Argonaut.DecodeJson a =>
  Data.Argonaut.EncodeJson a =>
  Data.Codec.Argonaut.JsonCodec a
fromArgonaut = Data.Codec.Argonaut.prismaticCodec from to Data.Codec.Argonaut.json
  where
  from :: Data.Argonaut.Json -> Maybe a
  from json = case Data.Argonaut.decodeJson json of
    Left _ -> Nothing
    Right x -> Just x

  to :: a -> Data.Argonaut.Json
  to = Data.Argonaut.encodeJson

-- | We provide a way to decode `argonaut`'s idea of JSON to a `View`.
fromJSON :: Data.Argonaut.Json -> Either String View
fromJSON json = case Data.Codec.Argonaut.decode view json of
  Left err -> Left (Data.Codec.Argonaut.printJsonDecodeError err)
  Right x -> Right x

-- | This is the main thing that decides how to de/serialize our values.
-- |
-- | Something we run into from time to time with separate de/encoders is them
-- | becoming out of sync.
-- | It's a _mostly_ caught by the test suite,
-- | but we can also allow the de/encoders to be correct by construction.
jsonView :: Data.Codec.Argonaut.JsonCodec JSONView
jsonView =
  Data.Codec.Argonaut.object "Lynx.List.JSONView"
    $ Data.Codec.Argonaut.recordProp (SProxy :: _ "columns") jsonColumns
    $ Data.Codec.Argonaut.record
  where
  jsonColumn :: Data.Codec.Argonaut.JsonCodec JSONColumn
  jsonColumn =
    Data.Codec.Argonaut.object "Lynx.List.JSONColumn"
      $ Data.Codec.Argonaut.recordProp (SProxy :: _ "name") fromArgonaut
      $ Data.Codec.Argonaut.recordProp (SProxy :: _ "value") Data.Codec.Argonaut.string
      $ Data.Codec.Argonaut.record

  jsonColumns :: Data.Codec.Argonaut.JsonCodec (Array JSONColumn)
  jsonColumns = Data.Codec.Argonaut.array jsonColumn

-- | We provide a way to encode a `View` to `argonaut`'s idea of JSON.
toJSON :: View -> Data.Argonaut.Json
toJSON view' = Data.Codec.Argonaut.encode view view'

-- | We provide a way to decode and encode the JSON representation of a `View`.
-- |
-- | The heavy lifting is done by the codec for `JSONView`.
-- | converting between the two types is done in a way that it can be typed.
view :: Data.Codec.Argonaut.JsonCodec View
view = Data.Codec.Argonaut.prismaticCodec from to jsonView
  where
  from :: JSONView -> Maybe View
  from jsonView' =
    Just
      { columns: jsonView'.columns
      }

  to :: View -> JSONView
  to view' =
    { columns: view'.columns
    }
