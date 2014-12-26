{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Blaze.React
    ( App(..)
    , WindowState(..), wsPath, wsBody

      -- * Handling window actions
    , WithWindowActions(..)
    , ignoreWindowActions

      -- * Writing transitions
    , Transition
    , TransitionM
    , runTransitionM
    , mkTransitionM
    , zoomTransition
    ) where

import Control.Lens (makeLenses, zoom, over, _2, LensLike')
import Control.Lens.Internal.Zoom (Focusing)
import Data.Functor.Identity (Identity)

import qualified Data.Text as T
import           Data.Typeable

import qualified Text.Blaze.Event           as E
import qualified Text.Blaze.Html5           as H

import           Control.Monad.State        (State, runState, get, put)
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell, mapWriterT)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data App state action = App
    { appInitialState    :: !state
    , appInitialRequests :: [IO action]
    , appApplyAction     :: !(action -> Transition state action)
    , appRender          :: !(state -> WindowState action)
    }

type Transition  state action = state -> (state, [IO action])
type TransitionM state action = WriterT [IO action] (State state) ()

data WindowState action = WindowState
    { _wsBody  :: !(H.Html action)
    , _wsPath  :: !T.Text
    -- TODO (asayers): _wsTitle :: T.Text
    }

data WithWindowActions act
    = PathChangedTo !T.Text
    | AppAction     !act
    deriving (Show, Typeable, Eq, Ord, Read)

makeLenses ''WindowState

-------------------------------------------------------------------------------
-- Handling window actions
-------------------------------------------------------------------------------

ignoreWindowActions :: App st act -> App st (WithWindowActions act)
ignoreWindowActions (App initialState initialRequests applyAction render) = App
    { appInitialState = initialState
    , appInitialRequests = (map (fmap AppAction) initialRequests)
    , appApplyAction = \action state -> case action of
        PathChangedTo _ -> (state, [])
        AppAction x   -> over _2 (map (fmap AppAction)) $ applyAction x state
    , appRender = over wsBody (E.mapActions AppAction) . render
    }

-------------------------------------------------------------------------------
-- Writing transitions
-------------------------------------------------------------------------------

runTransitionM :: TransitionM s a -> Transition s a
runTransitionM transition oldState =
    let ((_, reqs), newState) = runState (runWriterT transition) oldState
    in (newState, reqs)

mkTransitionM :: Transition s a -> TransitionM s a
mkTransitionM fn = do
    oldState <- get
    let (newState, writes) = fn oldState
    put newState
    tell writes

zoomTransition
    :: (innerA -> outerA)
    -> LensLike' (Focusing Identity ((), [IO outerA])) outerS innerS
       -- ^ This can be a @'Lens\'' innerS outerS@ or
       -- @'Traversal\'' innerS outerS@
    -> TransitionM innerS innerA
    -> TransitionM outerS outerA
zoomTransition wrapAction stateLens =
    mapWriterT $
      zoom stateLens .
        fmap -- State
          (fmap -- Pair
            (fmap -- List
              (fmap -- IO
                wrapAction)))
