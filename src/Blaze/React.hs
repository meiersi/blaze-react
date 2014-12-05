{-# LANGUAGE Rank2Types #-}

module Blaze.React
  ( App(..)
  , Transition
  , TransitionM
  , runTransitionM
  , mkTransitionM
  , zoomTransition
  ) where

import Control.Lens (Lens', zoom)

import qualified Text.Blaze.Html5           as H
import           Control.Monad.State        (State, runState, get, put)
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell, mapWriterT)

data App state action = App
    { appInitialState    :: state
    , appInitialRequests :: [IO action]
    , appApplyAction     :: action -> Transition state action
    , appRender          :: state -> H.Html action
    }

type Transition  state action = state -> (state, [IO action])
type TransitionM state action = WriterT [IO action] (State state) ()

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
    -> Lens' outerS innerS
    -- ^ Strictly, it's LensLike' (Focusing Identity ((), [IO outerA])) outerS innerS
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

