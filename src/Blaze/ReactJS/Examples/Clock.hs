{-# LANGUAGE OverloadedStrings #-}

module Blaze.ReactJS.Examples.Clock
    ( renderState
    , handleRequest
    ) where

import           Blaze.Core.Examples.Clock
import           Blaze.ReactJS.Base

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, forM_)

import           Data.Monoid ((<>))
import           Data.Time (getCurrentTime)

import qualified Text.Blaze.Html5 as H

renderState :: ClockS -> WindowState ClockA
renderState (ClockS mbTime) = WindowState
    { _wsPath = ""
    , _wsBody = case mbTime of
        Just time -> "The time is: " <> H.toHtml (show time)
        Nothing   -> "Loading..."
    }

handleRequest :: [ClockR] -> (ClockA -> IO ()) -> IO ()
handleRequest reqs chan = forM_ reqs $ \req ->
    case req of
      StartHeartbeat -> forever $ do
        time <- getCurrentTime
        chan $ TickA time
        threadDelay 1000000

