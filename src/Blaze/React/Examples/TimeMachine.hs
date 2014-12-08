{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  The time-machine app transformer
 -}

module Blaze.React.Examples.TimeMachine
    ( withTimeMachine
    ) where


import           Blaze.React

import           Control.Applicative
import           Control.Lens    (makeLenses, view, (.=), (%=), use, (+=))
import           Control.Monad
import           Control.Monad.Trans.Writer (tell)

import           Data.List       (foldl')
import           Data.Typeable   (Typeable)

import           Prelude hiding (div)

import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import           Text.Show.Pretty (ppShow)


-------------------------------------------------------------------------------
-- Time Machine
-------------------------------------------------------------------------------


-- state
--------

data TMState state action = TMState
    { _tmsInternalState :: state
    , _tmsActionHistory :: [action]
      -- ^ List of actions, from earliest to latest
    , _tmsActiveAction  :: Int
      -- ^ Index of the current position in the action list. 1-indexed,
      -- where 0 indicates that the app is in the initial state.
    , _tmsPaused        :: Bool
    , _tmsActionBuffer  :: [action]
      -- ^ This is where async internal actions go while the app is paused
    } deriving (Show)

makeLenses ''TMState


-- state transitions
--------------------

data TMAction action
    = TogglePauseAppA
    | ClearAppHistoryA
    | RevertAppHistoryA Int
    | InternalA action
    | AsyncInternalA action
    deriving (Eq, Ord, Read, Show, Typeable)

applyTMAction
    :: forall s a.
       s
    -> (a -> Transition s a)
    -> TMAction a
    -> Transition (TMState s a) (TMAction a)
applyTMAction initialInternalState applyInternalAction action =
    runTransitionM $ case action of
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
  where
    flushActionBuffer :: TransitionM (TMState s a) (TMAction a)
    flushActionBuffer = do
      buffer <- use tmsActionBuffer
      sequence_ $ map applyInternalAction' buffer
      tmsActionBuffer .= []

    -- | Apply an internal action to the internal state, adding it to the
    -- history, bumping the active action pointer, and possibly truncating
    -- the history first.
    applyInternalAction' :: a -> TransitionM (TMState s a) (TMAction a)
    applyInternalAction' act = do
      history      <- use tmsActionHistory
      activeAction <- use tmsActiveAction
      tmsActionHistory .= take activeAction history ++ [act]

      (internalState', reqs) <- applyInternalAction act <$> use tmsInternalState
      tmsInternalState .= internalState'
      tell $ fmap AsyncInternalA <$> reqs

      tmsActiveAction += 1


-- rendering
------------

renderTM :: (Show a, Show s) => (s -> H.Html a) -> TMState s a -> H.Html (TMAction a)
renderTM renderInternal state = do
    H.div H.! A.class_ "tm-time-machine" $ do
      H.h1 "Time machine"
      H.span H.! A.class_ "tm-button" H.! E.onClick' TogglePauseAppA $
        if (_tmsPaused state) then "Resume app" else "Pause app"
      H.span H.! A.class_ "tm-button" H.! E.onClick' ClearAppHistoryA $
        "Clear history"
      renderHistoryBrowser
      renderAppStateBrowser
    H.div H.! A.class_ "tm-internal-app" $
      E.mapActions InternalA $ renderInternal (view tmsInternalState state)
  where
    actionsWithIndices :: [(Int, String)]
    actionsWithIndices = reverse $
      (0, "Initial state") : (zip [1..] $ map show $ _tmsActionHistory state)

    renderHistoryBrowser = do
      H.h2 "Events"
      H.div H.! A.class_ "tm-history-browser" $ do
        H.ol $ forM_ actionsWithIndices $ \(idx, action) ->
          H.li H.! A.value (H.toValue $ idx + 1)
               H.! E.onMouseEnter (\_ -> RevertAppHistoryA idx)
               H.!? (idx == view tmsActiveAction state, A.class_ "tm-active-item")
               $ H.toHtml action

    renderAppStateBrowser = do
      H.h2 "Application state"
      H.div H.! A.class_ "tm-app-state-browser" $ H.pre $
        H.toHtml $ ppShow $ view tmsInternalState state


-- the application transformer
------------------------------

withTimeMachine
    :: (Show a, Show s)
    => App s a
    -> App (TMState s a) (TMAction a)
withTimeMachine internalApp = App
    { appInitialState    = initialTMState (appInitialState internalApp)
    , appInitialRequests = fmap AsyncInternalA <$> appInitialRequests internalApp
    , appApplyAction     = applyTMAction (appInitialState internalApp) (appApplyAction internalApp)
    , appRender          = renderTM (appRender internalApp)
    }

initialTMState :: s -> TMState s a
initialTMState internalState = TMState
    { _tmsInternalState = internalState
    , _tmsActionHistory = []
    , _tmsActiveAction  = 0       -- 0 indicates the initial state.
    , _tmsPaused        = False
    , _tmsActionBuffer  = []
    }
