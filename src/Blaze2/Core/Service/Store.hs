{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Blaze2.Core.Service.Store
  ( StoreA(..)
  , StoreR(..)
  ) where

import Data.Typeable
import           Test.QuickCheck (Arbitrary)

data StoreR v
    = ReadR
    | WriteR !v
    deriving (Eq, Ord, Show, Read, Typeable)

-- TODO (asayers): Consider adding ReadFailure
newtype StoreA v
    = ReadA v
    deriving (Eq, Ord, Show, Read, Typeable)


-- Instances
--------------

instance Arbitrary v => Arbitrary (StoreA v) where
