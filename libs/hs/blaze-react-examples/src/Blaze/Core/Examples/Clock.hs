{-# LANGUAGE DeriveDataTypeable #-}

module Blaze.Core.Examples.Clock
    ( app

    , ClockS(..)
    , ClockA(..)
    , ClockR(..)
    ) where

import           Blaze.Core
import           Data.Time (UTCTime)
import           Data.Typeable

data ClockS = ClockS !(Maybe UTCTime)
            deriving Show

data ClockA = TickA !UTCTime
            deriving (Show, Typeable)

data ClockR = StartHeartbeat
            deriving (Eq, Show)

applyAction :: ClockA -> ClockS -> (ClockS, [ClockR])
applyAction (TickA time) _ = (ClockS (Just time), [])

app :: App ClockS ClockA [ClockR]
app = App
    { appInitialState   = ClockS Nothing
    , appInitialRequest = [StartHeartbeat]
    , appApplyAction    = applyAction
    }

