{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for HTML-based and String-based applications, and a dummy
-- application for testing.
module Blaze.Development.Internal.Types
  ( RenderableApp(..)

  , HtmlApp
  , StringApp
  , htmlToStringApp

    -- * Testing
  , dummyHtmlApp
  , dummyStringApp
  ) where

import           Blaze.Core

import           Data.Monoid

import qualified Text.Blaze.Event             as E
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Renderer.String


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | An application that render to 'H.Html'.
data RenderableApp st act r = RenderableApp
    { raRender :: !(st -> r)
    , raApp    :: !(App st act (IORequest act))
    }
    deriving (Functor)

-- | An application that renders to 'String's. We use such applications for
-- testing purposes only.
type StringApp st act = RenderableApp st act String

type HtmlApp st act = RenderableApp st act (H.Html (E.EventHandler act))


htmlToStringApp :: HtmlApp st act -> StringApp st act
htmlToStringApp = fmap Text.Blaze.Renderer.String.renderHtml


------------------------------------------------------------------------------
-- Dummy HTML App for testing
------------------------------------------------------------------------------

dummyHtmlApp :: HtmlApp Integer ()
dummyHtmlApp = RenderableApp
    { raApp = App
        { appInitialState   = 0
        , appInitialRequest = mempty
        , appApplyAction    = \_act numClicks -> (succ numClicks, mempty)
        }
    , raRender = \numClicks ->
        H.h1 H.! E.onClick' ()
             $ "Number of clicks: " <> H.toHtml numClicks
    }

dummyStringApp :: StringApp Integer ()
dummyStringApp = htmlToStringApp dummyHtmlApp

