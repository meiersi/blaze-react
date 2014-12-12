{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Blaze.React.Examples.RoutingTest
    ( app
    ) where

import           Blaze.React

import qualified Data.Text                            as T
import           Data.Void

import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A

-- | We store the current path
type RTState = T.Text

-- | Only one allowed operation: changing the path
type RTAction = WithWindowActions Void

-- | Start with an empty path
initialRTState :: RTState
initialRTState = ""

-- | When the user requests a path change, we oblige.
applyRTAction :: RTAction -> Transition RTState RTAction
applyRTAction action _oldPath = case action of
    PathChangedTo path -> (path, [])
    AppAction _ -> undefined -- GHC's case analyzer seems to be unfamiliar with Void

renderRTState :: RTState -> WindowState RTAction
renderRTState path = WindowState
    { _wsPath = path                   -- ^ Display the current path
    , _wsBody = renderBody path
    }

renderBody :: RTState -> H.Html RTAction
renderBody path = do
    H.p "Type in the box to modify the path."
    H.p "Change the URL fragment and see the box update."
    H.p "Try pausing the app using the time machine and then changing the URL fragment."
    H.p $ H.input H.! A.type_ "text"
                  H.! A.value (H.toValue path)
                  H.! E.onValueChange PathChangedTo

app :: App RTState RTAction
app = App
    { appInitialState    = initialRTState
    , appInitialRequests = []
    , appApplyAction     = applyRTAction
    , appRender          = renderRTState
    }
