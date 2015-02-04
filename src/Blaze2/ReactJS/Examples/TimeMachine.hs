{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  The time-machine app transformer
 -}

module Blaze2.ReactJS.Examples.TimeMachine
    ( wrapRenderer
    , wrapHandler
    ) where


import           Blaze2.Core.Examples.TimeMachine

import           Blaze2.ReactJS.Base

import           Control.Lens    (view)
import           Control.Monad

import           Prelude hiding (div)

import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import           Text.Show.Pretty (ppShow)


-- request handling
--------------------

wrapHandler
    :: ((act -> IO ()) -> req -> IO ())
    -> (TMAction act -> IO ()) -> TMRequest req -> IO ()
wrapHandler handleInner channel =
    mapM_ $ handleInner (channel . AsyncInternalA)


-- rendering
------------

wrapRenderer
    :: (Show act, Show st)
    => (st -> WindowState act)
    -> TMState st act
    -> WindowState (TMAction act)
wrapRenderer renderInternalState state =
    let (WindowState internalBody internalPath) =
          renderInternalState (view tmsInternalState state)
    in WindowState
      { _wsPath = internalPath
      , _wsBody = renderBody internalBody state
      }

renderBody
    :: (Show act, Show st)
    => H.Html act
    -> TMState st act
    -> H.Html (TMAction act)
renderBody internalBody state = do
    H.div H.! A.class_ "tm-time-machine" $ do
      H.h1 "Time machine"
      H.span H.! A.class_ "tm-button" H.! E.onClick' TogglePauseAppA $
        if (_tmsPaused state) then "Resume app" else "Pause app"
      H.span H.! A.class_ "tm-button" H.! E.onClick' ClearAppHistoryA $
        "Clear history"
      renderHistoryBrowser
      renderAppStateBrowser
    H.div H.! A.class_ "tm-internal-app" $
      E.mapActions InternalA $ internalBody
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


