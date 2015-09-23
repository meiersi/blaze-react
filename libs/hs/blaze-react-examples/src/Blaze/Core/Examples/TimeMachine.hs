{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  The time-machine app transformer
 -}

module Blaze.Core.Examples.TimeMachine
    ( wrapApp

    , TMState(..), tmsInternalState, tmsActionHistory, tmsActiveAction
    , tmsPaused, tmsActionBuffer
    , TMAction(..)
    , TMRequest
    ) where


import           Blaze.Core

import           Control.Applicative
import           Control.Lens    (makeLenses, (.=), (%=), use, (+=), (<>=))
import           Control.Monad

import           Data.List       (foldl')
import           Data.Typeable   (Typeable)

import           Prelude hiding (div)


-------------------------------------------------------------------------------
-- Time Machine
-------------------------------------------------------------------------------

-- state
--------

data TMState st act req = TMState
    { _tmsInternalState  :: !st
    , _tmsActionHistory  :: [act]
      -- ^ List of actions, from earliest to latest
    , _tmsRequestHistory :: [req]
    , _tmsActiveAction   :: !Int
      -- ^ Index of the current position in the action list. 1-indexed,
      -- where 0 indicates that the app is in the initial state.
    , _tmsPaused         :: !Bool
    , _tmsActionBuffer   :: [act]
      -- ^ This is where async internal actions go while the app is paused
    } deriving (Show, Typeable)

makeLenses ''TMState


-- state transitions
--------------------

data TMAction act req
    = TogglePauseAppA
    | ClearAppHistoryA
    | RevertAppHistoryA Int
    | InternalA act
    | AsyncInternalA act
    | LogRequestA req
    deriving (Eq, Ord, Read, Show, Typeable)

type TMRequest req = [req]

applyTMAction
    :: forall st act req.
       st
    -> (act -> st -> (st, req))
    -> TMAction act req
    -> ApplyActionM (TMState st act req) (TMRequest req) ()
applyTMAction initialInternalState applyInternalAction action =
    case action of
      TogglePauseAppA -> do
        paused <- use tmsPaused
        when paused flushActionBuffer
        tmsPaused %= not
      ClearAppHistoryA -> do
        tmsActionHistory .= []
        tmsInternalState .= initialInternalState
        tmsActiveAction  .= 0
      RevertAppHistoryA idx -> do
        history' <- take idx <$> use tmsActionHistory
        -- Play back the recorded history, discarding the generated requests
        let internalState' = foldl' (\st act -> fst $ applyInternalAction act st)
                                initialInternalState history'
        tmsInternalState .= internalState'
        tmsActiveAction .= idx
      AsyncInternalA action' -> do
        paused <- use tmsPaused
        if paused
          then tmsActionBuffer %= (++ [action'])
          else applyInternalAction' action'
      InternalA action' -> do
        paused <- use tmsPaused
        unless paused $ applyInternalAction' action'
      LogRequestA req -> do
        tmsRequestHistory <>= [req]
  where
    flushActionBuffer :: ApplyActionM (TMState st act req) (TMRequest req) ()
    flushActionBuffer = do
      buffer <- use tmsActionBuffer
      sequence_ $ map applyInternalAction' buffer
      tmsActionBuffer .= []

    -- | Apply an internal action to the internal state, adding it to the
    -- history, bumping the active action pointer, and possibly truncating
    -- the history first.
    applyInternalAction'
        :: act -> ApplyActionM (TMState st act req) (TMRequest req) ()
    applyInternalAction' act = do
      history      <- use tmsActionHistory
      activeAction <- use tmsActiveAction
      tmsActionHistory .= take activeAction history ++ [act]

      (internalState', req) <- applyInternalAction act <$> use tmsInternalState
      tmsInternalState .= internalState'
      submitRequest [req]

      tmsActiveAction += 1

-- the application transformer
------------------------------

-- | If the inner app cares about WindowActions, be sure to wrap the wrapper
-- with @passWindowActionsThrough@ like so:
--
--     passWindowActionsThrough wrapApp innerApp
--
wrapApp :: (Show act, Show st)
     => App st act req
     -> App (TMState st act req) (TMAction act req) (TMRequest req)
wrapApp innerApp = App
    { appInitialState    = initialTMState initialInnerState
    , appInitialRequest  = [initialInnerReq]
    , appApplyAction     = runApplyActionM .
        applyTMAction initialInnerState applyInnerAction
    }
  where
    App initialInnerState initialInnerReq applyInnerAction = innerApp

initialTMState :: st -> TMState st act req
initialTMState internalState = TMState
    { _tmsInternalState  = internalState
    , _tmsActionHistory  = []
    , _tmsRequestHistory = []
    , _tmsActiveAction   = 0       -- 0 indicates the initial state.
    , _tmsPaused         = False
    , _tmsActionBuffer   = []
    }
