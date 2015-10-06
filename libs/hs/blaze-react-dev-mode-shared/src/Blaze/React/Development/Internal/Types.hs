{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for HTML-based and String-based applications, and a dummy
-- application for testing.
module Blaze.React.Development.Internal.Types
  ( RenderableApp(..)

  , HtmlApp
  , StringApp
  , htmlToStringApp

    -- * Testing
  , dummyHtmlApp
  , dummyStringApp
  ) where

import           Blaze.React.Core
import qualified Blaze.React.Html5                  as H
import qualified Blaze.React.Html5.Attributes       as A
import qualified Blaze.React.Html5.Event            as E
import qualified Blaze.React.Markup.Renderer.String as Renderer.String

import           Data.Monoid
import qualified Data.Text                    as T



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
htmlToStringApp = fmap Renderer.String.renderHtml


------------------------------------------------------------------------------
-- Dummy HTML App for testing
------------------------------------------------------------------------------

type DummyState     = (Integer, T.Text)
type DummyAction    = (Maybe T.Text)
type DummyHtmlApp   = HtmlApp DummyState DummyAction
type DummyStringApp = StringApp DummyState DummyAction

dummyHtmlApp :: DummyHtmlApp
dummyHtmlApp = RenderableApp
    { raApp = App
        { appInitialState   = (0, "change me")
        , appInitialRequest = mempty
        , appApplyAction    = \mbNewValue (numClicks, value) ->
              case mbNewValue of
                Nothing       -> ((succ numClicks, value), mempty)
                Just newValue -> ((numClicks, newValue), mempty)
        }
    , raRender = \(numClicks, value) ->
        ( H.h1 H.! E.onClick' Nothing
               $ "Number of clicks: " <> H.toHtml numClicks
        ) <>
        ( H.input H.! A.value (H.toValue value)
                  H.! E.onValueChange Just
        ) <>
        ( H.p $ H.toHtml $ T.reverse value )
    }

dummyStringApp :: DummyStringApp
dummyStringApp = htmlToStringApp dummyHtmlApp

