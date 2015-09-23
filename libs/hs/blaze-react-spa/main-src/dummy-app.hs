{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A simple test application for the blaze-react React.js output mode.
module Main
  ( main
  ) where

import           Blaze.Core
import           Blaze.ReactJS.Run

import           Data.Monoid

import qualified Text.Blaze.Event             as E
import qualified Text.Blaze.Html5             as H


------------------------------------------------------------------------------
-- Dummy HTML App for testing
------------------------------------------------------------------------------

type IOReq = (() -> IO ()) -> IO ()

dummyApp :: App Integer () IOReq
dummyApp = App
    { appInitialState   = 0
    , appInitialRequest = noRequest
    , appApplyAction    = \_act numClicks -> (succ numClicks, noRequest)
    }
  where
    noRequest = const $ return ()

dummyAppRender :: Integer -> H.Html (E.EventHandler ())
dummyAppRender numClicks =
    H.h1 H.! E.onClick' ()
         $ "Number of clicks: " <> H.toHtml numClicks

main :: IO ()
main = runApp' dummyAppRender dummyApp
