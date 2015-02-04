{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  The time-machine app transformer
 -}

module Blaze2.ReactJS.Examples.TimeMachine
    ( withTimeMachine
    ) where


import           Blaze2.Core
import           Blaze2.Core.Examples.TimeMachine

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



-- rendering
------------

renderTMState
    :: (Show a, Show s)
    => (s -> WindowState (WithWindowActions a))
    -> TMState s a
    -> WindowState (TMAction a)
renderTMState renderInternalState state =
    let (WindowState internalBody internalPath) =
          renderInternalState (view tmsInternalState state)
    in WindowState
      { _wsPath = internalPath
      , _wsBody = renderBody internalBody state
      }

renderBody
    :: (Show a, Show s)
    => H.Html (WithWindowActions a)
    -> TMState s a
    -> H.Html (TMAction a)
renderBody internalBody state = do
    H.div H.! A.class_ "tm-time-machine" $ do
      H.h1 "Time machine"
      H.span H.! A.class_ "tm-button" H.! E.onClick' (AppAction TogglePauseAppA) $
        if (_tmsPaused state) then "Resume app" else "Pause app"
      H.span H.! A.class_ "tm-button" H.! E.onClick' (AppAction ClearAppHistoryA) $
        "Clear history"
      renderHistoryBrowser
      renderAppStateBrowser
    H.div H.! A.class_ "tm-internal-app" $
      E.mapActions (AppAction . InternalA) $ internalBody
  where
    actionsWithIndices :: [(Int, String)]
    actionsWithIndices = reverse $
      (0, "Initial state") : (zip [1..] $ map show $ _tmsActionHistory state)

    renderHistoryBrowser = do
      H.h2 "Events"
      H.div H.! A.class_ "tm-history-browser" $ do
        H.ol $ forM_ actionsWithIndices $ \(idx, action) ->
          H.li H.! A.value (H.toValue $ idx + 1)
               H.! E.onMouseEnter (\_ -> AppAction $ RevertAppHistoryA idx)
               H.!? (idx == view tmsActiveAction state, A.class_ "tm-active-item")
               $ H.toHtml action

    renderAppStateBrowser = do
      H.h2 "Application state"
      H.div H.! A.class_ "tm-app-state-browser" $ H.pre $
        H.toHtml $ ppShow $ view tmsInternalState state


