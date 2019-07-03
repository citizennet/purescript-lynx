module Lynx.List
  ( Column
  , Input
  , Query
  , Output
  , Size(..)
  , View
  , component
  , fromJSON
  , toJSON
  ) where

import Prelude
import Data.Argonaut as Data.Argonaut
import Data.Array as Data.Array
import Data.Codec.Argonaut as Data.Codec.Argonaut
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Either as Data.Either
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable as Data.Traversable
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Lynx.Expr as Lynx.Expr
import Lynx.Form as Lynx.Form
import Ocelot.Block.Card as Ocelot.Block.Card
import Ocelot.Block.Format as Ocelot.Block.Format
import Ocelot.Block.Table as Ocelot.Block.Table
import Ocelot.HTML.Properties as Ocelot.HTML.Properties

-- | The intent of providing `JSONView` as an abstraction instead of doing
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
    , width :: Size
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
    , width :: Size
    }

-- | The representation of a "view" that we want to render.
-- | This representation is also different from the `View` so we can make
-- | changes to how we render things without forcing those changes to ripple
-- | back into the logic of what a "view" really does.
type RenderView
  = { columns :: Array RenderColumn
    , rows :: Array (Lynx.Form.Page Lynx.Expr.ExprType)
    }

type RenderColumn
  = { name :: Lynx.Expr.ExprType
    , value :: Lynx.Expr.Key
    , width :: Size
    }

-- | Representation of various sizing grades for use in UI
data Size
  = Auto
  | Small
  | Medium
  | Large

derive instance eqSize :: Eq Size

derive instance genericSize :: Generic Size _

instance showSize :: Show Size where
  show = genericShow

instance encodeJsonSize :: Data.Argonaut.EncodeJson Size where
  encodeJson =
    Data.Argonaut.encodeJson
      <<< case _ of
          Auto -> "Auto"
          Small -> "Small"
          Medium -> "Medium"
          Large -> "Large"

instance decodeJsonSize :: Data.Argonaut.DecodeJson Size where
  decodeJson json = do
    str <- Data.Argonaut.decodeJson json
    case str of
      "Auto" -> pure Auto
      "Small" -> pure Small
      "Medium" -> pure Medium
      "Large" -> pure Large
      _ -> Left (str <> " is not a valid Size")

type Input
  = { columns :: Array Column
    , rows :: Array (Lynx.Form.Page Lynx.Expr.ExprType)
    }

type Query
  = Const Void

type Output
  = Void

type State
  = Either Lynx.Expr.EvalError RenderView

component :: forall f. Halogen.Component Halogen.HTML.HTML Query Input Output f
component =
  Halogen.component
    { eval
    , initialState
    , receiver
    , render
    }

eval :: forall f. Query ~> f
eval query = case query of
  Const x -> absurd x

fromArgonaut ::
  forall a.
  Data.Argonaut.DecodeJson a =>
  Data.Argonaut.EncodeJson a =>
  Data.Codec.Argonaut.JsonCodec a
fromArgonaut = Data.Codec.Argonaut.prismaticCodec from to Data.Codec.Argonaut.json
  where
  from :: Data.Argonaut.Json -> Maybe a
  from json = Data.Either.hush (Data.Argonaut.decodeJson json)

  to :: a -> Data.Argonaut.Json
  to = Data.Argonaut.encodeJson

-- | We provide a way to decode `argonaut`'s idea of JSON to a `View`.
fromJSON :: Data.Argonaut.Json -> Either String View
fromJSON json = case Data.Codec.Argonaut.decode view json of
  Left err -> Left (Data.Codec.Argonaut.printJsonDecodeError err)
  Right x -> Right x

initialState :: Input -> State
initialState input = do
  columns <- Data.Traversable.traverse column input.columns
  pure
    { columns
    , rows: input.rows
    }
  where
  column :: Column -> Either Lynx.Expr.EvalError RenderColumn
  column column' = do
    name <- Lynx.Expr.evalExpr lookup column'.name
    pure
      { name
      , value: column'.value
      , width: column'.width
      }

  lookup :: Lynx.Expr.Key -> Maybe Lynx.Expr.ExprType
  lookup key = Nothing

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
      $ Data.Codec.Argonaut.recordProp (SProxy :: _ "width") fromArgonaut
      $ Data.Codec.Argonaut.record

  jsonColumns :: Data.Codec.Argonaut.JsonCodec (Array JSONColumn)
  jsonColumns = Data.Codec.Argonaut.array jsonColumn

receiver :: Input -> Maybe (Query Unit)
receiver input = Nothing

render :: forall a f. State -> Halogen.HTML a f
render state' = case state' of
  Left error ->
    Ocelot.Block.Card.card_
      [ Ocelot.Block.Format.p
          [ Ocelot.HTML.Properties.css "text-red"
          ]
          [ evalError error
          ]
      ]
  Right state ->
    Ocelot.Block.Table.table_
      $ [ Ocelot.Block.Table.row_ (map column state.columns)
        ]
      <> map (row state.columns) state.rows
  where
  cell :: Lynx.Form.Page Lynx.Expr.ExprType -> RenderColumn -> Halogen.HTML a f
  cell page column' = Ocelot.Block.Table.cell_ (cellValue page column'.value)

  cellValue :: Lynx.Form.Page Lynx.Expr.ExprType -> Lynx.Expr.Key -> Array (Halogen.HTML a f)
  cellValue page key =
    Data.Array.fromFoldable do
      field <- Lynx.Form.getField key page
      exprType <- case field.input of
        Lynx.Form.Currency currency -> Lynx.Form.getValue currency
        Lynx.Form.DateTime dateTime -> Lynx.Form.getValue dateTime
        Lynx.Form.Dropdown dropdown -> Lynx.Form.getValue dropdown
        Lynx.Form.Text text -> Lynx.Form.getValue text
        Lynx.Form.Toggle toggle -> Lynx.Form.getValue toggle
        Lynx.Form.TypeaheadSingle typeaheadSingle -> Lynx.Form.getValue typeaheadSingle
      pure (Halogen.HTML.text $ Lynx.Expr.print exprType)

  column :: RenderColumn -> Halogen.HTML a f
  column column' =
    Ocelot.Block.Table.header
      (columnProps column')
      [ Halogen.HTML.text (Lynx.Expr.print column'.name)
      ]

  columnProps :: RenderColumn -> _
  columnProps column' = case column'.width of
    Auto -> mempty
    Small -> [ Ocelot.HTML.Properties.css "w-1" ]
    Medium -> [ Ocelot.HTML.Properties.css "w-1/6" ]
    Large -> [ Ocelot.HTML.Properties.css "w-5/6" ]

  evalError :: Lynx.Expr.EvalError -> Halogen.HTML a f
  evalError error = case error of
    Lynx.Expr.IfCondition x ->
      Halogen.HTML.text
        $ "Expected conditional to be a Boolean, but its type is: "
        <> Lynx.Expr.reflectType x
    Lynx.Expr.EqualMismatch x ->
      Halogen.HTML.text
        $ "Expected both sides of equal to have the same type,"
        <> " but they are different."
        <> " left: "
        <> Lynx.Expr.reflectType x.left
        <> " right: "
        <> Lynx.Expr.reflectType x.right

  row :: Array RenderColumn -> Lynx.Form.Page Lynx.Expr.ExprType -> Halogen.HTML a f
  row columns page =
    Ocelot.Block.Table.row_
      (map (cell page) columns)

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
