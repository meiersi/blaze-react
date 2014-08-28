module Blaze.React
  ( App(..)
  ) where

import qualified Text.Blaze.Html5 as H

data App state action = App
    { appInitialState    :: state
    , appInitialRequests :: [IO action]
    , appApplyAction     :: action -> state -> (state, [IO action])
    , appRender          :: state -> H.Html action
    }
