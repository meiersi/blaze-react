{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-
  The time-machine app transformer
 -}

module TimeMachine
    ( TMEventHandler(..)
    , withTimeMachine
    ) where

import           Prelude hiding (div)

import           Control.Applicative
import           Control.Lens    (makeLenses, view, set, over)
import           Control.Monad

import           Data.List       (foldl')
import           Data.Time       (UTCTime)

import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A

import           Text.Show.Pretty (ppShow)

import           TodoApp (App(..), DOMEvent(..))


-------------------------------------------------------------------------------
-- Time Machine
-------------------------------------------------------------------------------

data TMState state action = TMState
    { _tmsInternalState :: state
    , _tmsActionHistory :: [action]
      -- ^ List of actions, from earliest to latest
    , _tmsActiveAction  :: Int
      -- ^ Index of the current position in the action list. 1-indexed,
      -- where 0 indicates that the app is in the initial state.
    , _tmsPaused        :: Bool
    }

data TMAction action
    = TogglePauseAppA
    | RevertAppHistoryA Int
    | InternalA action
    deriving (Eq, Ord, Read, Show)

data TMEventHandler eventHandler
    = TogglePauseAppEH
    | ActionHistoryItemEH Int
    | InternalEH eventHandler
    deriving (Eq, Ord, Read, Show)

makeLenses ''TMState

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

renderTM :: (Show a, Show s) => (s -> H.Html eh) -> TMState s a -> H.Html (TMEventHandler eh)
renderTM renderInternal state = do
    H.div H.! A.class_ "tm-time-machine" $ do
      H.h1 "Time machine"
      H.div H.! A.class_ "tm-button" H.! H.onEvent TogglePauseAppEH $
        if (_tmsPaused state) then "Resume app" else "Pause app"
      renderHistoryBrowser
      renderAppStateBrowser
    H.div H.! A.class_ "tm-internal-app" $
      H.mapEventHandlers InternalEH $ renderInternal (view tmsInternalState state)
  where
    actionsWithIndices :: [(Int, String)]
    actionsWithIndices =
      (0, "Initial state") : (zip [1..] $ map show $ _tmsActionHistory state)

    renderHistoryBrowser = do
      H.h2 "Events"
      H.div H.! A.class_ "tm-history-browser" $ do
        H.ol $ forM_ actionsWithIndices $ \(idx, action) ->
          H.li H.! H.onEvent (ActionHistoryItemEH idx)
               H.!? (idx == view tmsActiveAction state, A.class_ "tm-active-item")
               $ H.toHtml action

    renderAppStateBrowser = do
      H.h2 "Application state"
      H.div H.! A.class_ "tm-app-state-browser" $ H.pre $
        H.toHtml $ ppShow $ view tmsInternalState state


handleTMEvent
    :: (UTCTime -> DOMEvent -> eh -> Maybe a)
    -> UTCTime -> DOMEvent -> TMEventHandler eh -> Maybe (TMAction a)
handleTMEvent handleInternalEvent utcTime domEvent handler =
    case (handler, domEvent) of
      (TogglePauseAppEH, OnClick) -> Just TogglePauseAppA
      (TogglePauseAppEH, _      ) -> Nothing
      (ActionHistoryItemEH idx, OnClick) -> Just $ RevertAppHistoryA idx
      (ActionHistoryItemEH _, _        ) -> Nothing
      (InternalEH handler', _) ->
        InternalA <$> handleInternalEvent utcTime domEvent handler'

initialTMState :: s -> TMState s a
initialTMState internalState = TMState
    { _tmsInternalState = internalState
    , _tmsActionHistory = []
    , _tmsActiveAction  = 0       -- ^ 0 indicates the initial state.
    , _tmsPaused        = False
    }

withTimeMachine
    :: (Show a, Show s) => App s a eh
    -> App (TMState s a) (TMAction a) (TMEventHandler eh)
withTimeMachine internalApp = App
    { appInitialState = initialTMState (appInitialState internalApp)
    , appApplyAction  = applyTMAction (appInitialState internalApp) (appApplyAction internalApp)
    , appRender       = renderTM (appRender internalApp)
    , appHandleEvent  = handleTMEvent (appHandleEvent internalApp)
    }

