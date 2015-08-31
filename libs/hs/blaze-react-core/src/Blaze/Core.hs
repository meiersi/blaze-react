{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
module Blaze.Core
    ( App(..)

      -- * Support for writing the state transitions
    , ApplyActionM
    , runApplyActionM
    , submitRequest
    , readState
    , writeState
    , zoomTransition

      -- * Some useful app transformers
    , (:+:)
    , ignoreActions
    , passActionsThrough

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


-- | This type is the heart of the blaze-react API. In short, an `App` is a
-- state machine which can make external requests. Three things need to be
-- defined:
--
-- - The machine's state-space. This is captured in the `st` type parameter.
-- - The possible transitions. This is captured in the `act` type parameter.
-- - The interface against which the machine can make external requests. This
--   is captured by the `req` type parameter.
--
-- A machine must have an initial state, and we need to know what effect the
-- possible transitions should have. The `act` type is expected to be an
-- enumeration of the ways in which the outside world can affect your
-- application's internal state, although it is perfectly possible to just use
-- `st -> st` as your action type.
--
-- These `App`s differ from garden-variety state machines in only one way: when
-- making a state transition, they are allowed to emit requests. Typically,
-- these requests are captured, some IO is performed as a result, and some kind
-- of return value is given by applying an action to the machine.
--
-- Thus, the `act` and `req` types give the interface through which your `App`s
-- interact with the world: I/O becomes `act`/`req`. This differs from standard
-- IO in two regards:
--
-- 1. The model is completely asyncronous. This better suits certain targets,
--    such as browsers.
-- 2. The app logic is written against an opaque interface. All
--    platform-dependent code is contained in an implementation, which can be
--    easily swapped out. This means that application logic is completely
--    portable, and, as a result, easily testable.
data App st act req = App
    { appInitialState   :: !st
    , appInitialRequest :: !req
    , appApplyAction    :: !(act -> st -> (st, req))
    }

-- instances
------------

instance Functor (App st act) where
    fmap f (App st0 req0 apply) =
        App st0 (f req0) (\act st -> over _2 f (apply act st))

instance Profunctor (App st) where
    dimap g f (App st0 req0 apply) =
        App st0 (f req0) (\act st -> over _2 f (apply (g act) st))


-- Helpers for writing state transitions
-------------------------------------------------------------------------------

type ApplyActionM st req = WriterT req (State st)

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


-- Some useful app transformers
-------------------------------------------------------------------------------

-- | Allow easy anonymous sum types
--
-- FIXME (asayers): Clearly this doesn't belong here. Perhaps it'll go in
-- Data.Either, once TypeOperators makes its way into the Haskell Report...
type (:+:) = Either

-- NOTE: Originally I used a `Monoid req` constraint, but I want to be able to
-- use this function on Apps with request type `((act -> IO ()) -> IO ())`, and
-- IO () is not a monoid.
ignoreActions
    :: req
    -> App st                  act  req
    -> App st (irrelevantA :+: act) req
ignoreActions emptyReq app = app
    { appApplyAction = \action -> case action of
          Left _  -> \state -> (state, emptyReq)
          Right x -> appApplyAction app x
    }

-- | This functions allows you to lift cases from the action type of an app
-- into the action type of an app which wraps it. For instance, if an app knows
-- how to handle WindowActions, then any app which wraps it also knows how to
-- handle WindowActions - it can just pass them through to the inner app. That
-- is what this function does.
passActionsThrough
   :: ((act :+: innnerA) -> outerA)
   -> App st          outerA  req
   -> App st (act :+: outerA) req
passActionsThrough wrapAction app = app
    { appApplyAction = \action -> case action of
          Left  x -> appApplyAction app (wrapAction $ Left x)
          Right x -> appApplyAction app x
    }


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

