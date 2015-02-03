{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module Blaze2.React.Service.Base
  ( ServiceR(..)
  , ServiceA(..)
  ) where

data ServiceA api = forall a b. ServiceA (api a b) a b

data ServiceR api = forall a b. ServiceR (api a b) a
