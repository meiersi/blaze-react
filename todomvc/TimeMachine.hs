{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
  The time-machine app transformer
 -}

module TimeMachine
    ( withTimeMachine
    ) where

import           Prelude hiding (div)

import           Control.Applicative
import           Control.Lens    (makeLenses, view, set, over, _1)
import           Control.Monad

import           Data.List       (foldl')

import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A

import           Text.Show.Pretty (ppShow)

import           TodoApp (App(..))


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
    , _tmsActionBuffer  :: [action]
      -- ^ This is where async internal actions go while the app is paused
    }

data TMAction action
    = TogglePauseAppA
    | RevertAppHistoryA Int
    | InternalA action
    | AsyncInternalA action
    deriving (Eq, Ord, Read, Show)

makeLenses ''TMState

applyTMAction
    :: forall s a. s -> (a -> s -> (s, [IO a]))
    -> TMAction a -> TMState s a -> (TMState s a, [IO (TMAction a)])
applyTMAction initialInternalState applyInternalAction action state =
    case action of
      TogglePauseAppA
        | paused    -> set (_1 . tmsPaused) False $ flushActionBuffer state
        | otherwise -> noRequests $ set tmsPaused True state
      RevertAppHistoryA idx -> noRequests $
        let history'       = take idx $ view tmsActionHistory state
            internalState' = foldl' (\st act -> fst $ applyInternalAction act st)
                                initialInternalState history'
        in set tmsInternalState internalState' $ set tmsActiveAction idx $ state
      AsyncInternalA action'
        | paused    -> noRequests $ over tmsActionBuffer (++ [action']) state
        | otherwise -> applyInternalAction' state action'
      InternalA action'
        | paused    -> noRequests state
        | otherwise -> applyInternalAction' state action'
  where
    paused = view tmsPaused state

    noRequests x = (x, [])

    flushActionBuffer :: TMState s a -> (TMState s a, [IO (TMAction a)])
    flushActionBuffer = go []
      where
        go reqs st = case view tmsActionBuffer st of
          []     -> (st, reqs)
          (x:xs) -> let (st', reqs') = applyInternalAction' st x
                    in go (reqs ++ reqs') $ set tmsActionBuffer xs $ st'

    -- | Apply an internal action to the internal state, adding it to the
    -- history, bumping the active action pointer, and possibly truncating
    -- the history first.
    applyInternalAction' :: TMState s a -> a -> (TMState s a, [IO (TMAction a)])
    applyInternalAction' (TMState internalState history activeAction p b) act =
      let history'
            | activeAction == length history = history ++ [act]
            | otherwise                      = take activeAction history ++ [act]
          (internalState', reqs) = applyInternalAction act internalState
          state' = TMState internalState' history' (activeAction + 1) p b
          reqs' = fmap AsyncInternalA <$> reqs
      in (state', reqs')

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

initialTMState :: s -> TMState s a
initialTMState internalState = TMState
    { _tmsInternalState = internalState
    , _tmsActionHistory = []
    , _tmsActiveAction  = 0       -- ^ 0 indicates the initial state.
    , _tmsPaused        = False
    , _tmsActionBuffer  = []
    }

withTimeMachine
    :: (Show a, Show s) => App s a
    -> App (TMState s a) (TMAction a)
withTimeMachine internalApp = App
    { appInitialState    = initialTMState (appInitialState internalApp)
    , appInitialRequests = fmap AsyncInternalA <$> appInitialRequests internalApp
    , appApplyAction     = applyTMAction (appInitialState internalApp) (appApplyAction internalApp)
    , appRender          = renderTM (appRender internalApp)
    }

