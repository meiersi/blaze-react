module Blaze.React
  ( App(..)
  ) where

import qualified Text.Blaze.Html5 as H

data App state action = App
    { appInitialState :: state
    , appApplyAction  :: action -> state -> state
    , appRender       :: state -> H.Html action
    }
