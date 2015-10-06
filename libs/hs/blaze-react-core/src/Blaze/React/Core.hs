{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Core abstractions for blaze-react: applications, transitions, and
-- IORequests.
module Blaze.React.Core
    ( App(..)

      -- * Support for writing the state transitions
    , ApplyActionM
    , runApplyActionM
    , submitRequest
    , readState
    , writeState
    , zoomTransition

      -- * Support for testing apps
    , testApp

      -- * Support for IORequests
    , IORequest
    , ioRequest
    , runIORequest
    , performIO_
    , concurrently
    ) where

import           Control.Lens (zoom, over, _2, LensLike')
import           Control.Lens.Internal.Zoom (Zoomed)

import           Control.Concurrent                (forkIO)
import           Control.Monad                     (void)
import           Control.Monad.Cont                (ContT(..))
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Trans               (lift)
import           Control.Monad.Trans.State.Strict  (State, runState, get, put)
import           Control.Monad.Trans.Writer.Strict (WriterT, execWriterT, tell, mapWriterT)

#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid      (Monoid())
#endif
import           Data.Profunctor            (Profunctor(dimap))
import           Data.Tuple                 (swap)


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


------------------------------------------------------------------------------
-- IO Requests
------------------------------------------------------------------------------

-- FIXME (SM): understand this type better and move it to a proper place.


-- | A request to the environment to execute some IO and possibly apply some
-- actions to the state.
newtype IORequest act = IORequest { unIORequest :: ContT () IO act }
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Execute an 'IORequest' with the given callback.
runIORequest :: IORequest act -> (act -> IO ()) -> IO ()
runIORequest = runContT . unIORequest

-- | Construct an 'IORequest'.
ioRequest :: ((act -> IO ()) -> IO ()) -> IORequest act
ioRequest = IORequest . ContT

-- | Perform some IO as part of executing that 'IORequest'.
performIO_ :: IO () -> IORequest act
performIO_ io = ioRequest $ \_k -> io

-- | Execute this IORequest concurrently.
concurrently :: IORequest act -> IORequest act
concurrently r1 = ioRequest $ \k -> void $ forkIO $ runIORequest r1 k


-- instances
------------

instance Monoid (IORequest act) where
    mempty          = ioRequest $ \_k -> return ()
    r1 `mappend` r2 =
        ioRequest $ \k  -> runIORequest r1 k >> runIORequest r2 k



