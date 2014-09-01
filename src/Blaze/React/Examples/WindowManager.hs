{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Blaze.React.Examples.WindowManager
    (
      NamedApp
    , namedApp

    , windowManager
    ) where

import           Blaze.React      (App(..))

import           Control.Applicative
import           Control.Lens         hiding (act)
import           Control.Monad        (when)

import           Data.Monoid      (mconcat)
import qualified Data.Text        as T
import           Data.Foldable    (foldMap)
import           Data.Typeable    (Typeable, cast)

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


------------------------------------------------------------------------------
-- NamedApps
------------------------------------------------------------------------------

-- | A NamedApp is an application which can be instantiated by the window
-- manager.
data NamedApp = forall st act.
    (Typeable act, Show act, Show st) => NamedApp !T.Text (App st act)

instance Show NamedApp where
    showsPrec prec (NamedApp name _) =
      showsPrec prec ("NamedApp" :: T.Text, name)

instantiateNamedApp :: NamedApp -> (WindowState, [IO WindowAction])
instantiateNamedApp (NamedApp name (App q0 reqs0 apply render)) =
    (WindowState name q0 apply render, map (fmap WindowAction) reqs0)

------------------------------------------------------------------------------
-- Wrapping up different applications and their actions
------------------------------------------------------------------------------

-- Runtime type information for the win! :-)

data WindowState = forall st act. (Typeable act, Show act, Show st) => WindowState
    { winName    :: !T.Text
    , _winState  :: !st
    , _winApply  :: !(act -> st -> (st, [IO act]))
    , _winRender :: !(st -> H.Html act)
    }

data WindowAction = forall act. (Typeable act, Show act) => WindowAction act


-- instances
------------

instance Show WindowState where
    showsPrec prec (WindowState name st _ _) =
        showsPrec prec ("WindowState" :: T.Text, name, st)

instance Show WindowAction where
    showsPrec prec (WindowAction act) = showsPrec prec act


-- operations
-------------

fromWindowAction :: Typeable act => WindowAction -> Maybe act
fromWindowAction (WindowAction act) = cast act

applyWindowAction :: WindowAction -> WindowState -> (WindowState, [IO WindowAction])
applyWindowAction someAct someApp@(WindowState name st apply render) =
    case fromWindowAction someAct of
      Nothing  -> (someApp, []) -- ignore actions from other apps
                                -- TODO (meiersi): log this as a bug
      Just act ->
        let (st', reqs) = apply act st
        in  (WindowState name st' apply render, map (fmap WindowAction) reqs)

renderWindowContents :: WindowState -> H.Html WindowAction
renderWindowContents (WindowState _name st _apply render) =
    H.mapActions WindowAction $ render st


------------------------------------------------------------------------------
-- Combining multiple apps using a tabbed switcher
------------------------------------------------------------------------------

data WMAction
    -- = SwitchWorkspace  !Int
    = DestroyWindow !Int   -- ^ windowIdx
    | CreateWindow  !Int   -- ^ appIdx
    | ToggleCreateMenu
    | AppAction  !Int WindowAction
    deriving (Show, Typeable)

data WMState = WMState
    -- FIXME (asayers): we need to give windows a stable reference for use
    -- in AppAction constructors. Right now, if you close a window which
    -- has created a request, the resulting action will be applied to the
    -- next window in the list. Not good.
    { _wmsWindows         :: [WindowState]
    , _wmsApps            :: [NamedApp]
    , _wmsShowCreateMenu  :: !Bool
    } deriving (Show)

-- data WorkspaceState = WorkspaceState
--    { _wssWindows        :: [Int]   -- ^ the windows which are in this workspace
--    } deriving (Show)

makeLenses ''WMState
-- makeLenses ''WorkspaceState


applyWMAction
    :: WMAction -> WMState -> (WMState, [IO WMAction])
applyWMAction act st = case act of
    -- SwitchWorkspace workspaceIdx
    --   | nullOf (wmsWorkspaces . ix workspaceIdx) st -> (st, [])
    --   | otherwise -> (set wmsActiveWorkspace workspaceIdx st, [])

    DestroyWindow windowIdx ->
      (over wmsWindows (deleteAt windowIdx) st, [])

    CreateWindow appIdx ->
      case preview (wmsApps . ix appIdx) st of
        Nothing  -> (st, [])
        Just app ->
          let (window, reqs) = instantiateNamedApp app
          in  ( set wmsShowCreateMenu False $ over wmsWindows (++ [window]) st
              , map (fmap $ AppAction $ length $ view wmsWindows st) reqs
              )

    ToggleCreateMenu -> (over wmsShowCreateMenu not st, [])

    AppAction windowIdx windowAction ->
      case preview (wmsWindows . ix windowIdx) st of
        Nothing     -> (st, [])
        Just window ->
          let (window', reqs) = applyWindowAction windowAction window
          in ( set (wmsWindows . ix windowIdx) window' st
             , fmap (AppAction windowIdx) <$> reqs
             )
  where
    -- Why isn't this in Data.List?
    deleteAt :: Int -> [a] -> [a]
    deleteAt _ []     = []
    deleteAt 0 (_:xs) = xs
    deleteAt n (x:xs) = x : deleteAt (n - 1) xs


renderWMState :: WMState -> H.Html WMAction
renderWMState (WMState windows apps showCreateMenu) = do
    H.div H.! A.class_ "tabbed-app-picker" $ do
      H.span H.! A.class_ "tabbed-create-button" H.! H.onClick ToggleCreateMenu $ "[+]"
    when showCreateMenu $ H.div H.! A.class_ "tabbed-create-menu" $
      H.ul $ foldMap createItem $ zip [0..] apps
    H.div H.! A.class_ "tabbed-internal-app" $
      renderWorkspace $ zip [0..] windows
  where
    renderWorkspace :: [(Int, WindowState)] -> H.Html WMAction
    renderWorkspace windows =
        H.div H.! A.class_ "wm-workspace" $ case windows of
          []     -> ""
          [x]    -> renderWindow 100 100 x
          (x:xs) -> do
            H.div H.! A.class_ "wm-right-col" $
              mconcat $ renderWindow 50 (100 `div` length xs) <$> xs
            H.div H.! A.class_ "wm-left-col" $ renderWindow 50 100 x

    renderWindow :: Int -> Int -> (Int, WindowState) -> H.Html WMAction
    renderWindow width height (windowIdx, window) =
      H.div H.! A.class_ "wm-window" $ do
        H.div H.! A.class_ "wm-title-bar" $ do
          H.span $ H.toHtml $ winName window
          H.span H.! A.class_ "wm-close-button" H.! H.onClick (DestroyWindow windowIdx) $ "[x]"
        H.div $ H.mapActions (AppAction windowIdx) $ renderWindowContents window
          -- H.! A.style ("width: " <> H.toValue width <> "vw;\
          --                 \ height: " <> H.toValue height <> "vh;") $

    createItem (appIdx, NamedApp name _) =
      H.li H.! H.onClick (CreateWindow appIdx) $ H.toHtml name


-------------------------------------------------------------------------------
-- Public interface
-------------------------------------------------------------------------------

namedApp :: (Typeable act, Show act, Show st) => T.Text -> App st act -> NamedApp
namedApp = NamedApp

windowManager :: [NamedApp] -> App WMState WMAction
windowManager apps = App
    { appInitialState    = WMState [] apps False
    , appInitialRequests = []
    , appApplyAction     = applyWMAction
    , appRender          = renderWMState
    }

