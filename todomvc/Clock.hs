{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Clock
    ( clockApp
    ) where

import           Prelude hiding (div)

import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Lens    (makeLenses, set)

import           Data.Monoid     ((<>))
import           Data.Time       (UTCTime, getCurrentTime)

import qualified Text.Blaze.Html5                     as H

import           TodoApp (App(..), DOMEvent(..))


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data ClockState = ClockState
    { _csTime :: !(Maybe UTCTime)
    } deriving Show

data ClockAction = TickA UTCTime deriving Show
data ClockEventHandler

deriving instance Show ClockEventHandler
deriving instance Read ClockEventHandler

makeLenses ''ClockState


-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

renderClockState :: ClockState -> H.Html ClockEventHandler
renderClockState (ClockState (Just time)) = "The time is: " <> H.toHtml (show time)
renderClockState (ClockState Nothing)     = "Loading..."

applyClockAction :: ClockAction -> ClockState -> (ClockState, [IO ClockAction])
applyClockAction action state = case action of
    TickA time -> (set csTime (Just time) state, [scheduleTick])

scheduleTick :: IO ClockAction
scheduleTick = do
  threadDelay 1000000
  TickA <$> getCurrentTime

handleClockEvent :: UTCTime -> DOMEvent -> ClockEventHandler -> Maybe ClockAction
handleClockEvent _time _domEvent _eventHandler = Nothing

clockApp :: App ClockState ClockAction ClockEventHandler
clockApp = App
    { appInitialState    = ClockState { _csTime = Nothing }
    , appInitialRequests = [scheduleTick]
    , appApplyAction     = applyClockAction
    , appRender          = renderClockState
    , appHandleEvent     = handleClockEvent
    }

