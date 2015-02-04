{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
module Blaze2.Core
  ( App(..)
  , (:+:)

    -- * Support for writing the state transitions
  , ApplyActionM
  , runApplyActionM
  , submitRequest
  , readState
  , writeState
  , zoomTransition

    -- * Support for testing apps
  , testApp
  ) where

import           Control.Lens (zoom, over, _2, LensLike')
import           Control.Lens.Internal.Zoom (Zoomed)

import           Control.Monad.Trans               (lift)
import           Control.Monad.Trans.State.Strict  (State, runState, get, put)
import           Control.Monad.Trans.Writer.Strict (WriterT, execWriterT, tell, mapWriterT)

import           Data.Monoid      (Monoid())
import           Data.Profunctor  (Profunctor(dimap))
import           Data.Tuple       (swap)


-- | Allow eas
type (:+:) = Either

-- Helpers for writing state transitions
-------------------------------------------------------------------------------

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

zoomTransition
    :: (innerR -> outerR)
    -> LensLike' (Zoomed (State outerS) ((), outerR)) outerS innerS
       -- ^ This this unifies with:
       -- - @'Lens\'' innerS outerS@
       -- - @'Traversal\'' innerS outerS@
    -> ApplyActionM innerS innerR ()
    -> ApplyActionM outerS outerR ()
zoomTransition wrapRequest stateLens =
    mapWriterT $
      zoom stateLens .
        fmap -- State
          (fmap -- Pair
              wrapRequest)

-- instances
------------

instance Functor (App st act) where
    fmap f (App st0 req0 apply) =
        App st0 (f req0) (\act st -> over _2 f (apply act st))

instance Profunctor (App st) where
    dimap g f (App st0 req0 apply) =
        App st0 (f req0) (\act st -> over _2 f (apply (g act) st))


-- Helpers for writing tests
-------------------------------------------------------------------------------

testApp
    :: (Show act)
    => (st -> Bool)
    -> (req -> [act])
    -> App st act req
    -> [act]
    -> Bool
testApp validState reqToActs app =
    go (appInitialState app)
  where
    go st acts0 =
        validState st &&
        case acts0 of
          []         -> True
          (act:acts) ->
            case appApplyAction app act st of
              (st', req) -> go st' (reqToActs req ++ acts)

