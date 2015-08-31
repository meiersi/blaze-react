{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  The time-machine app transformer
 -}

module Blaze.ReactJS.Examples.TimeMachine
    ( wrapRenderer
    , wrapHandler
    ) where


import           Blaze.Core.Examples.TimeMachine

import           Blaze.ReactJS.Base

import           Control.Lens    (view)
import           Control.Monad

import           Data.Monoid

import           Prelude hiding (div)

import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import           Text.Show.Pretty (ppShow)


-- request handling
--------------------

-- TODO (asayers): This is actually platform-independent, so it can be moved to
-- Core.Examples.TimeMachine.
--
-- FIXME (asayers): Imposing Eq on the request type is a bit ugly. Perhaps it's
-- better to take `isNull :: req -> Bool` as a parameter?
wrapHandler
    :: (Monoid req, Eq req)
    => (req -> (act -> IO ()) -> IO ())
    -> TMRequest req
    -> (TMAction act req -> IO ())
    -> IO ()
wrapHandler handleInner reqs channel =
    forM_ reqs $ \req -> do
        unless (req == mempty) $ channel $ LogRequestA req
        handleInner req (channel . AsyncInternalA)


-- rendering
------------

wrapRenderer
    :: (Show act, Show st, Show req)
    => (st -> WindowState act)
    -> TMState st act req
    -> WindowState (TMAction act req)
wrapRenderer renderInternalState state =
    let (WindowState internalBody internalPath) =
          renderInternalState (view tmsInternalState state)
    in WindowState
      { _wsPath = internalPath
      , _wsBody = renderBody internalBody state
      }

renderBody
    :: (Show act, Show st, Show req)
    => H.Html act
    -> TMState st act req
    -> H.Html (TMAction act req)
renderBody internalBody state = do
    H.div H.! A.class_ "tm-time-machine" $ do
      H.h1 "Time machine"
      H.span H.! A.class_ "tm-button" H.! E.onClick' TogglePauseAppA $
        if (_tmsPaused state) then "Resume app" else "Pause app"
      H.span H.! A.class_ "tm-button" H.! E.onClick' ClearAppHistoryA $
        "Clear history"
      renderActionHistory
      renderRequestHistory
      renderAppStateBrowser
    H.div H.! A.class_ "tm-internal-app" $
      E.mapActions InternalA $ internalBody
  where
    actionsWithIndices :: [(Int, String)]
    actionsWithIndices = reverse $
      (0, "Initial state") : (zip [1..] $ map show $ _tmsActionHistory state)

    requestsWithIndices :: [(Int, String)]
    requestsWithIndices = reverse $
      zip [1..] $ map show $ _tmsRequestHistory state

    renderActionHistory = do
      H.h2 "Actions"
      H.div H.! A.class_ "tm-history-browser" $ do
        H.ol $ forM_ actionsWithIndices $ \(idx, action) ->
          H.li H.! A.value (H.toValue $ idx + 1)
               H.! E.onMouseEnter (\_ -> RevertAppHistoryA idx)
               H.!? (idx == view tmsActiveAction state, A.class_ "tm-active-item")
               $ H.toHtml action

    renderRequestHistory = do
      H.h2 "Requests"
      H.div H.! A.class_ "tm-history-browser" $ do
        H.ol $ forM_ requestsWithIndices $ \(idx, request) ->
          H.li H.! A.value (H.toValue idx)
               $ H.toHtml request

    renderAppStateBrowser = do
      H.h2 "Application state"
      H.div H.! A.class_ "tm-app-state-browser" $ H.pre $
        H.toHtml $ ppShow $ view tmsInternalState state


