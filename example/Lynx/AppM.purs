module Lynx.AppM where

import Prelude
import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Type.Equality (class TypeEquals, from)

-- todo
type Env
  = {}

newtype AppM a
  = AppM (ReaderT Env Aff a)

-- function to recover Aff from our custom monad, as Halogen
-- components must run in Aff
runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive instance newtypeAppM :: Newtype (AppM a) _

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from
