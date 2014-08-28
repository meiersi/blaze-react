{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-
  The time-machine app transformer
 -}

module Blaze.React.Examples.TimeMachine
    ( withTimeMachine
    ) where


import           Blaze.React (App(..))

import           Control.Lens    (makeLenses, view, set, over)
import           Control.Monad

import           Data.List       (foldl')

import           Prelude hiding (div)

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
    }

makeLenses ''TMState


-- state transitions
--------------------

data TMAction action
    = TogglePauseAppA
    | RevertAppHistoryA Int
    | InternalA action
    deriving (Eq, Ord, Read, Show)


applyTMAction :: s -> (a -> s -> s) -> TMAction a -> TMState s a -> TMState s a
applyTMAction initialInternalState applyInternalAction action state =
    case action of
      TogglePauseAppA -> over tmsPaused not state
      RevertAppHistoryA idx ->
        let history'       = take idx $ view tmsActionHistory state
            internalState' = foldl' (flip applyInternalAction) initialInternalState history'
        in set tmsInternalState internalState' $ set tmsActiveAction idx $ state
      InternalA action'
        | view tmsPaused state -> state
        | otherwise            ->
            let state' = if (_tmsActiveAction state) == length (_tmsActionHistory state)
                           then state
                           else over tmsActionHistory (take (_tmsActiveAction state)) state
            in over tmsActionHistory (++ [action']) $
            over tmsActiveAction (+ 1) $
            over tmsInternalState (applyInternalAction action') $
            state'


-- rendering
------------

renderTM :: (Show a, Show s) => (s -> H.Html a) -> TMState s a -> H.Html (TMAction a)
renderTM renderInternal state = do
    H.div H.! A.class_ "tm-time-machine" $ do
      H.h1 "Time machine"
      H.div H.! A.class_ "tm-button" H.! H.onClick TogglePauseAppA $
        if (_tmsPaused state) then "Resume app" else "Pause app"
      renderHistoryBrowser
      renderAppStateBrowser
    H.div H.! A.class_ "tm-internal-app" $
      H.mapActions InternalA $ renderInternal (view tmsInternalState state)
  where
    actionsWithIndices :: [(Int, String)]
    actionsWithIndices =
      (0, "Initial state") : (zip [1..] $ map show $ _tmsActionHistory state)

    renderHistoryBrowser = do
      H.h2 "Events"
      H.div H.! A.class_ "tm-history-browser" $ do
        H.ol $ forM_ actionsWithIndices $ \(idx, action) ->
          H.li H.! H.onMouseOver (RevertAppHistoryA idx)
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
    { appInitialState = initialTMState (appInitialState internalApp)
    , appApplyAction  = applyTMAction (appInitialState internalApp) (appApplyAction internalApp)
    , appRender       = renderTM (appRender internalApp)
    }

initialTMState :: s -> TMState s a
initialTMState internalState = TMState
    { _tmsInternalState = internalState
    , _tmsActionHistory = []
    , _tmsActiveAction  = 0       -- ^ 0 indicates the initial state.
    , _tmsPaused        = False
    }
