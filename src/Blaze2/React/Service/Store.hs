{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Blaze2.React.Service.Store
  ( StoreA(..)
  , StoreR(..)
  ) where

import Data.Typeable

data StoreR v
    = ReadR
    | WriteR !v
    deriving (Eq, Ord, Show, Read, Typeable)

newtype StoreA v
    = ReadA v
    deriving (Eq, Ord, Show, Read, Typeable)

