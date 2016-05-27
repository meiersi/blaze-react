{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

-- | TODO (SM): this module should go into a different library, as render
-- functions and application transitions must be able to depend on it.
module Blaze.React.Backend.ReactJS.Base
    ( WindowState(..), wsBody, wsPath
    , WindowAction(..)
    ) where

import           Control.Lens          (makeLenses)

import qualified Data.Text             as T
import           Data.Typeable         (Typeable)

import           Prelude               hiding (div)

import qualified Blaze.React.Html5       as H
import qualified Blaze.React.Html5.Event as E


data WindowState act = WindowState
    { _wsBody  :: !(H.Html (E.EventHandler act))
    , _wsPath  :: !T.Text
      -- TODO (asayers): _wsTitle :: T.Text
    }

data WindowAction
    = PathChangedTo !T.Text
    deriving (Show, Typeable, Eq, Ord, Read)

makeLenses ''WindowState

