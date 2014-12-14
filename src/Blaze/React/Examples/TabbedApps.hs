{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Blaze.React.Examples.TabbedApps
    (
      NamedApp
    , namedApp
    , namedApp'

    , tabbed
    ) where

import           Blaze.React

import           Control.Applicative
import           Control.Lens         hiding (act)

import           Data.Foldable    (foldMap)
import           Data.Monoid      ((<>))
import qualified Data.Text        as T
import qualified Data.Text.Read   as T
import           Data.Typeable    (Typeable, cast)

import qualified Text.Blaze.Event            as E
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


------------------------------------------------------------------------------
-- Wrapping up different applications and their actions
------------------------------------------------------------------------------

-- Runtime type information for the win! :-)


data SomeApp = forall st act. (Typeable act, Show act, Show st) => SomeApp
    { saName   :: !T.Text
    , _saState  :: !st
    , _saApply  :: !(WithWindowActions act -> Transition st (WithWindowActions act))
    , _saRender :: !(st -> WindowState (WithWindowActions act))
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

toSomeAction
    :: (Typeable act, Show act)
    => WithWindowActions act
    -> WithWindowActions SomeAction
toSomeAction act = case act of
    PathChangedTo x -> PathChangedTo x
    AppAction     x -> AppAction $ SomeAction x

fromSomeAction
    :: Typeable act
    => WithWindowActions SomeAction
    -> Maybe (WithWindowActions act)
fromSomeAction act = case act of
    PathChangedTo x                -> Just $ PathChangedTo x
    AppAction (SomeAction someAct) -> AppAction <$> cast someAct

applySomeAction
    :: WithWindowActions SomeAction
    -> SomeApp
    -> (SomeApp, [IO (WithWindowActions SomeAction)])
applySomeAction someAct someApp@(SomeApp name st apply render) =
    case fromSomeAction someAct of
      Nothing  -> (someApp, []) -- ignore actions from other apps
                                -- TODO (meiersi): log this as a bug
      Just act ->
        let (st', reqs) = apply act st
        in  (SomeApp name st' apply render, map (fmap toSomeAction) reqs)

renderSomeApp :: SomeApp -> WindowState (WithWindowActions SomeAction)
renderSomeApp (SomeApp _name st _apply render) =
    over wsBody (E.mapActions toSomeAction) $ render st

------------------------------------------------------------------------------
-- Combining multiple apps using a tabbed switcher
------------------------------------------------------------------------------

type TabbedAction = WithWindowActions TabbedAction'
data TabbedAction'
    = SwitchApp !Int
    | InnerA    !Int (WithWindowActions SomeAction)
    deriving (Show, Typeable)

data TabbedState = TabbedState
   { _tsFocus     :: Int
   , _tsApps      :: [SomeApp]
   -- invariants:
   --   - 0 <= tsFocus < length tsApps
   } deriving (Show)

makeLenses ''TabbedState


-- Applying actions
--------------------

applyTabbedAction
    :: TabbedAction -> Transition TabbedState TabbedAction
applyTabbedAction act st = case act of
    PathChangedTo path -> case preview pathToRoute path of
      Nothing                             -> (st, [])
      Just (TabbedRoute appIdx innerPath) -> flip runTransitionM st $ do
        mkTransitionM $ applyTabbedAction $ AppAction $ SwitchApp appIdx
        mkTransitionM $ applyTabbedAction $ AppAction $ InnerA appIdx $ PathChangedTo innerPath

    AppAction (SwitchApp appIdx)
      | nullOf (tsApps . ix appIdx) st -> (st, [])
      | otherwise                      -> (set tsFocus appIdx st, [])

    AppAction (InnerA appIdx someAction) ->
      case preview (tsApps . ix appIdx) st of
        Nothing      -> (st, [])
        Just someApp ->
          let (someApp', reqs) = applySomeAction someAction someApp
          in ( set (tsApps . ix appIdx) someApp' st
             , fmap (AppAction . InnerA appIdx) <$> reqs
             )

-- Routing
-----------

data TabbedRoute = TabbedRoute
    { _trAppIdx :: Int
    , _trInnerPath :: T.Text
    }

pathToRoute :: Prism' T.Text TabbedRoute
pathToRoute = prism' fromRoute toRoute
  where
    fromRoute :: TabbedRoute -> T.Text
    fromRoute (TabbedRoute appIdx innerPath) = T.concat
        [ T.pack $ show appIdx
        , if T.null innerPath
            then ""
            else "-" <> innerPath
        ]

    toRoute :: T.Text -> Maybe TabbedRoute
    toRoute path =
        case T.decimal path of
          Left _                   -> Nothing
          Right (appIdx, leftover) -> Just $ TabbedRoute
            { _trAppIdx = appIdx
            , _trInnerPath = T.drop 1 leftover
            }

-- Rendering
-------------

renderTabbedState :: TabbedState -> WindowState TabbedAction
renderTabbedState state@(TabbedState focusedAppIdx apps) =
    case preview (ix focusedAppIdx) apps of
      Nothing  -> error "renderTabbedState: no app focused"
      Just app ->
        let (WindowState innerBody innerPath) = renderSomeApp app
        in WindowState
          { _wsPath = review pathToRoute $ TabbedRoute focusedAppIdx innerPath
          , _wsBody = renderBody state innerBody
          }

renderBody :: TabbedState -> H.Html (WithWindowActions SomeAction) -> H.Html TabbedAction
renderBody (TabbedState focusedAppIdx apps) innerBody = do
    H.div H.! A.class_ "tabbed-app-picker" $
      foldMap appItem $ zip [0..] apps
    H.div H.! A.class_ "tabbed-internal-app" $
      E.mapActions (AppAction . InnerA focusedAppIdx) innerBody
  where
    appItem (appIdx, app) =
      H.span H.!? (focusedAppIdx == appIdx, A.class_ "tabbed-active-item")
             H.! E.onClick' (AppAction $ SwitchApp appIdx) $ H.toHtml $ saName app


------------------------------------------------------------------------------
-- Bundling up several applications
------------------------------------------------------------------------------

data NamedApp = forall st act. (Typeable act, Show act, Show st)
              => NamedApp !T.Text (App st (WithWindowActions act))

namedAppInitialRequests :: (Int, NamedApp) -> [IO TabbedAction]
namedAppInitialRequests (appIdx, NamedApp _ (App _q0 reqs0 _apply _render)) =
    map (fmap (AppAction . InnerA appIdx . toSomeAction)) reqs0

namedAppToSomeApp :: NamedApp -> SomeApp
namedAppToSomeApp (NamedApp name (App q0 _reqs0 apply render)) =
    SomeApp name q0 apply render

namedApp
    :: (Typeable act, Show act, Show st)
    => T.Text
    -> App st (WithWindowActions act)
    -> NamedApp
namedApp = NamedApp

namedApp'
    :: (Typeable act, Show act, Show st)
    => T.Text
    -> App st act
    -> NamedApp
namedApp' name = namedApp name . ignoreWindowActions

-- FIXME (asayers): Routing isn't handled well for internal apps
tabbed :: Int -> [NamedApp] -> App TabbedState TabbedAction
tabbed initialAppIdx apps = App
    { appInitialState    = TabbedState initialAppIdx (namedAppToSomeApp <$> apps)
    , appInitialRequests = concatMap namedAppInitialRequests $ zip [0..] apps
    , appApplyAction     = applyTabbedAction
    , appRender          = renderTabbedState
    }

