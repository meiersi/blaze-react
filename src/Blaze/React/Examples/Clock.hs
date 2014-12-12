{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A clock app which demonstrates the ability to spawn threads which
-- transform the state when they return.
module Blaze.React.Examples.Clock
    ( app
    ) where

import           Prelude hiding (div)

import           Blaze.React

import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Lens       (makeLenses, set, view)

import           Data.Monoid        ((<>))
import           Data.Time          (UTCTime, getCurrentTime)
import           Data.Typeable      (Typeable)

import qualified Text.Blaze.Html5   as H


-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------

data ClockState = ClockState
    { _csTime :: !(Maybe UTCTime)
    } deriving Show

makeLenses ''ClockState

renderClockState :: ClockState -> WindowState ClockAction
renderClockState state = WindowState
    { _wsPath = ""
    , _wsBody = case view csTime state of
        Just time -> "The time is: " <> H.toHtml (show time)
        Nothing   -> "Loading..."
    }

-------------------------------------------------------------------------------
-- Transitions
-------------------------------------------------------------------------------

data ClockAction
    = TickA UTCTime
    deriving (Show, Typeable)

applyClockAction :: ClockAction -> ClockState -> (ClockState, [IO ClockAction])
applyClockAction action state = case action of
    TickA time -> (set csTime (Just time) state, [scheduleTick])

scheduleTick :: IO ClockAction
scheduleTick = do
  threadDelay 1000000
  TickA <$> getCurrentTime

-------------------------------------------------------------------------------
-- App
-------------------------------------------------------------------------------

app :: App ClockState ClockAction
app = App
    { appInitialState    = ClockState { _csTime = Nothing }
    , appInitialRequests = [scheduleTick]
    , appApplyAction     = applyClockAction
    , appRender          = renderClockState
    }

