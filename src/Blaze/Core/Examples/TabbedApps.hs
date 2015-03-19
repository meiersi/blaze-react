{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Blaze.Core.Examples.TabbedApps
    ( wrapApps
    , SomeApp(..)
    , SomeState(..)
    , SomeAction(..)
    , SomeRequest(..)

    , TabbedS(..)
    , TabbedA(..)
    , TabbedR(..)
    ) where

import           Blaze.Core

import           Control.Lens  (ix, set, preview)

import           Data.Typeable (Typeable, cast)
import qualified Data.Text     as T

data SomeApp
    = forall st act req.
      (Typeable st, Typeable act, Typeable req, Show st, Show req)
    => SomeApp
      { _saName :: !T.Text
      , _saApp  :: !(App st act req)
      }

data SomeState
    = forall st act req.
      (Typeable st, Typeable act, Typeable req, Show st, Show req)
    => SomeState
      { _ssName  :: !T.Text
      , _ssState :: !st
      , _ssApply :: (act -> st -> (st, req))
      }
data SomeAction  = forall act. (Typeable act, Show act) => SomeAction  !act
data SomeRequest = forall req. (Typeable req, Show req) => SomeRequest !req

instance Show SomeState where
    showsPrec prec (SomeState name st _) =
        showsPrec prec ("SomeApp", name, st)
deriving instance Show SomeAction
deriving instance Show SomeRequest

data TabbedA = InnerA  !Int !SomeAction | SwitchFocusA !Int deriving Show
data TabbedR = InnerR [(Int, SomeRequest)]                  deriving Show
data TabbedS = TabbedS
    { _tsFocus  :: !Int
    , _tsStates :: [SomeState]
    } deriving Show

applySomeAction :: SomeAction -> SomeState -> (SomeState, SomeRequest)
applySomeAction (SomeAction someAction) (SomeState name state apply) =
    case cast someAction of
      Nothing ->
        error "Action type and app type do not match"
      Just action ->
        let (state', request) = apply action state
        in (SomeState name state' apply, SomeRequest request)

applyTabbedAction :: TabbedA -> TabbedS -> (TabbedS, TabbedR)
applyTabbedAction action (TabbedS focus states) =
    case action of
      SwitchFocusA focus'
        | focus' >= length states -> (TabbedS focus  states, InnerR [])
        | otherwise               -> (TabbedS focus' states, InnerR [])
      InnerA appIdx innerAction ->
        case preview (ix appIdx) states of
          Nothing ->
            error $ "applyTabbedAction: App no. " ++
                    show appIdx ++ " doesn't exist"
          Just state ->
            let (state', request) = applySomeAction innerAction state
            in ( TabbedS focus (set (ix appIdx) state' states)
               , InnerR [(appIdx, request)]
               )

appsToInitialRequest :: [SomeApp] -> TabbedR
appsToInitialRequest apps = InnerR $ do
    (idx, SomeApp _ (App _ initialRequest _)) <- zip [0..] apps
    return (idx, SomeRequest initialRequest)

appsToInitialState :: [SomeApp] -> TabbedS
appsToInitialState apps = TabbedS 0 $ do
    SomeApp name (App initialState _ apply) <- apps
    return $ SomeState name initialState apply

wrapApps :: [SomeApp] -> App TabbedS TabbedA TabbedR
wrapApps apps
    | length apps <= 0 = error "wrapApps: No apps given"
    | otherwise = App
        { appInitialState   = appsToInitialState apps
        , appInitialRequest = appsToInitialRequest apps
        , appApplyAction    = applyTabbedAction
        }

