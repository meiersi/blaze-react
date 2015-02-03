{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
module Blaze2.React
  ( App(..)
  , (:+:)

    -- * Support for writing the state transitions
  , ApplyActionM
  , runApplyActionM
  , submitRequest
  , readState
  , writeState
  ) where

import           Control.Lens                      (_2, over)
import           Control.Monad.Trans               (lift)
import           Control.Monad.Trans.State.Strict  (State, runState, get, put)
import           Control.Monad.Trans.Writer.Strict (WriterT, execWriterT, tell)

import           Data.Monoid      (Monoid())
import           Data.Tuple       (swap)
import           Data.Profunctor  (Profunctor(dimap))


-- | Allow eas
type (:+:) = Either

type ApplyActionM st req = WriterT req (State st)

data App st act req = App
    { appInitialState   :: !st
    , appInitialRequest :: !req
    , appApplyAction    :: !(act -> st -> (st, req))
    }

runApplyActionM :: ApplyActionM st req () -> (st -> (st, req))
runApplyActionM m = swap . runState (execWriterT m)

submitRequest :: Monoid req => req -> ApplyActionM st req ()
submitRequest = tell

readState :: Monoid req => ApplyActionM st req st
readState = lift get

writeState :: Monoid req => st -> ApplyActionM st req ()
writeState = lift . put


-- instances
------------

instance Functor (App st act) where
    fmap f (App st0 req0 apply) =
        App st0 (f req0) (\act st -> over _2 f (apply act st))

instance Profunctor (App st) where
    dimap g f (App st0 req0 apply) =
        App st0 (f req0) (\act st -> over _2 f (apply (g act) st))

