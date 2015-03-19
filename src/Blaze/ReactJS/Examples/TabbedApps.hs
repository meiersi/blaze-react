{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Blaze.ReactJS.Examples.TabbedApps
    ( wrapRenderers
    , SomeRenderer(..)

    , wrapHandlers
    , SomeHandler(..)
    ) where

import           Blaze.Core.Examples.TabbedApps
import           Blaze.ReactJS.Base

import           Control.Lens                (preview, ix)

import           Data.Foldable               (foldMap)
import qualified Data.HashMap.Strict         as HMS
import           Data.Typeable               (Typeable, cast)

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Event            as E


data SomeRenderer
    = forall st act. (Typeable st, Typeable act, Show act)
    => SomeRenderer (st -> WindowState act)

wrapRenderers
    :: [SomeRenderer]
    -> TabbedS
    -> WindowState TabbedA
wrapRenderers renderers tabbedState@(TabbedS focus states) =
    case preview (ix focus) (zip states renderers) of
      Nothing -> error "wrapRenderers: app not found"
      Just (SomeState _ someState _, SomeRenderer render) ->
        case cast someState of
          Nothing -> error "wrapRenderers: renderer and state don't match"
          Just focusedState ->
            let (WindowState innerBody innerPath) = render focusedState
            in WindowState
              { _wsPath = innerPath -- TODO (asayers): add focus prefix
              , _wsBody = renderBody tabbedState innerBody
              }

renderBody
    :: (Typeable act, Show act)
    => TabbedS -> H.Html act -> H.Html TabbedA
renderBody (TabbedS focus states) innerBody = do
    H.div H.! A.class_ "tabbed-app-picker" $
      foldMap (uncurry appItem) $ zip [0..] states
    H.div H.! A.class_ "tabbed-internal-app" $
      E.mapActions (InnerA focus . SomeAction) innerBody
  where
    appItem appIdx app = H.span
      H.!? (focus == appIdx, A.style (HMS.fromList [("color", "#eee")]))
      H.! E.onClick' (SwitchFocusA appIdx) $ H.toHtml $ _ssName app


data SomeHandler
    = forall act req. (Typeable act, Typeable req, Show act)
    => SomeHandler (req -> (act -> IO ()) -> IO ())

wrapHandlers :: [SomeHandler] -> TabbedR -> (TabbedA -> IO ()) -> IO ()
wrapHandlers handlers (InnerR reqs) chan =
    sequence_ $ do
      (appIdx, SomeRequest someRequest) <- reqs
      case preview (ix appIdx) handlers of
        Nothing -> error "wrapHandlers: couldn't find handler"
        Just (SomeHandler handle) ->
          case cast someRequest of
            Nothing -> error "wrapHandlers: request and handler don't match"
            Just request ->
              return $ handle request (chan . InnerA appIdx . SomeAction)




