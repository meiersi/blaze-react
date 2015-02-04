{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Blaze2.ReactJS.Base
    ( WindowState(..), wsBody, wsPath
    , WindowAction(..)
    , ignoreWindowActions
    ) where

import           Blaze2.Core

import           Control.Applicative
import           Control.Concurrent        (threadDelay, forkIO)
import           Control.Exception         (bracket)
import           Control.Lens              (makeLenses)
import           Control.Monad

import           Data.IORef
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           (Monoid, mempty)
import qualified Data.Text             as T
import           Data.Typeable

import           Prelude hiding (div)

import qualified Text.Blaze.Html5               as H


data WindowState act = WindowState
    { _wsBody  :: !(H.Html act)
    , _wsPath  :: !T.Text
      -- TODO (asayers): _wsTitle :: T.Text
    }

data WindowAction
    = PathChangedTo !T.Text
    deriving (Show, Typeable, Eq, Ord, Read)

makeLenses ''WindowState

ignoreWindowActions
    :: Monoid req => App st act req -> App st (Either WindowAction act) req
ignoreWindowActions app = app
    { appApplyAction = \action -> case action of
        Left (PathChangedTo _) -> \state -> (state, mempty)
        Right innerAction      -> appApplyAction app innerAction
    }

