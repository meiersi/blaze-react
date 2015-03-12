{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module Blaze.ReactJS.Base
    ( WindowState(..), wsBody, wsPath
    , WindowAction(..)
    ) where

import           Control.Lens          (makeLenses)

import qualified Data.Text             as T
import           Data.Typeable         (Typeable)

import           Prelude               hiding (div)

import qualified Text.Blaze.Html5      as H


data WindowState act = WindowState
    { _wsBody  :: !(H.Html act)
    , _wsPath  :: !T.Text
      -- TODO (asayers): _wsTitle :: T.Text
    }

data WindowAction
    = PathChangedTo !T.Text
    deriving (Show, Typeable, Eq, Ord, Read)

makeLenses ''WindowState

