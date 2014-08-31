{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Blaze.React.Examples.TabbedApps
    (
      NamedApp
    , namedApp

    , tabbed
    ) where

import           Blaze.React      (App(..))

import           Control.Applicative
import           Control.Lens         hiding (act)

import qualified Data.Text        as T
import           Data.Foldable    (foldMap)
import           Data.Typeable    (Typeable, cast)

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


------------------------------------------------------------------------------
-- Wrapping up different applications and their actions
------------------------------------------------------------------------------

-- Runtime type information for the win! :-)


data SomeApp = forall st act. (Typeable act, Show act, Show st) => SomeApp
    { saName   :: !T.Text
    , _saState  :: !st
    , _saApply  :: !(act -> st -> (st, [IO act]))
    , _saRender :: !(st -> H.Html act)
    }

data SomeAction = forall act. (Typeable act, Show act) => SomeAction act


-- instances
------------

instance Show SomeApp where
    showsPrec prec (SomeApp name st _ _) =
        showsPrec prec ("SomeApp" :: T.Text, name, st)

instance Show SomeAction where
    showsPrec prec (SomeAction act) = showsPrec prec act


-- operations
-------------

fromSomeAction :: Typeable act => SomeAction -> Maybe act
fromSomeAction (SomeAction act) = cast act

applySomeAction :: SomeAction -> SomeApp -> (SomeApp, [IO SomeAction])
applySomeAction someAct someApp@(SomeApp name st apply render) =
    case fromSomeAction someAct of
      Nothing  -> (someApp, []) -- ignore actions from other apps
                                -- TODO (meiersi): log this as a bug
      Just act ->
        let (st', reqs) = apply act st
        in  (SomeApp name st' apply render, map (fmap SomeAction) reqs)

renderSomeApp :: SomeApp -> H.Html SomeAction
renderSomeApp (SomeApp _name st _apply render) =
    H.mapActions SomeAction $ render st


------------------------------------------------------------------------------
-- Combining multiple apps using a tabbed switcher
------------------------------------------------------------------------------

data TabbedAction
    = SwitchApp !Int
    | AppAction !Int SomeAction
    deriving (Show, Typeable)

data TabbedState = TabbedState
   { _tsFocus     :: Int
   , _tsApps      :: [SomeApp]
   -- invariants:
   --   - 0 <= tsFocus < length tsApps
   } deriving (Show)

makeLenses ''TabbedState


applyTabbedAction
    :: TabbedAction -> TabbedState -> (TabbedState, [IO TabbedAction])
applyTabbedAction act st = case act of
    SwitchApp appIdx
      | nullOf (tsApps . ix appIdx) st -> (st, [])
      | otherwise                      -> (set tsFocus appIdx st, [])

    AppAction appIdx someAction ->
      case preview (tsApps . ix appIdx) st of
        Nothing      -> (st, [])
        Just someApp ->
          let (someApp', reqs) = applySomeAction someAction someApp
          in ( set (tsApps . ix appIdx) someApp' st
             , fmap (AppAction appIdx) <$> reqs
             )


renderTabbedState :: TabbedState -> H.Html TabbedAction
renderTabbedState (TabbedState focusedAppIdx apps) = do
    H.div H.! A.class_ "tabbed-app-picker" $
      foldMap appItem $ zip [0..] apps
    H.div H.! A.class_ "tabbed-internal-app" $
      case preview (ix focusedAppIdx) apps of
        Nothing  -> "invariant violation: no app focused"
        Just app -> H.mapActions (AppAction focusedAppIdx) $ renderSomeApp app
  where
    appItem (appIdx, app) =
      H.span H.!? (focusedAppIdx == appIdx, A.class_ "tabbed-active-item")
             H.! H.onClick (SwitchApp appIdx) $ H.toHtml $ saName app


------------------------------------------------------------------------------
-- Bundling up several applications
------------------------------------------------------------------------------

data NamedApp = forall st act.
    (Typeable act, Show act, Show st) => NamedApp !T.Text (App st act)

namedAppInitialRequests :: (Int, NamedApp) -> [IO TabbedAction]
namedAppInitialRequests (appIdx, NamedApp _ (App _q0 reqs0 _apply _render)) =
    map (fmap (AppAction appIdx . SomeAction)) reqs0

namedAppToSomeApp :: NamedApp -> SomeApp
namedAppToSomeApp (NamedApp name (App q0 _reqs0 apply render)) =
    SomeApp name q0 apply render

namedApp :: (Typeable act, Show act, Show st) => T.Text -> App st act -> NamedApp
namedApp = NamedApp

tabbed :: Int -> [NamedApp] -> App TabbedState TabbedAction
tabbed initialAppIdx apps = App
    { appInitialState    =
        TabbedState initialAppIdx (namedAppToSomeApp <$> apps)
    , appInitialRequests = concatMap namedAppInitialRequests $ zip [0..] apps
    , appApplyAction     = applyTabbedAction
    , appRender          = renderTabbedState
    }

