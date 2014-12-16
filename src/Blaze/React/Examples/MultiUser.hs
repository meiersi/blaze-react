{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This app transformer equips an application with the ability to have
-- multiple seats. WARNING: We're not doing anything special here with regard
-- to security. Every user's data is visible.
module Blaze.React.Examples.MultiUser
    ( withMultiUser
    ) where

import Blaze.React

import Control.Applicative ((<$>))
import Control.Lens        (makeLenses, at, (^.), (.=), use, non, view)

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           Data.Typeable       (Typeable)

import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Event.Keycode             as Keycode
import qualified Text.Blaze.Html5    as H
import qualified Text.Blaze.Html5.Attributes          as A


newtype Username = Username { unUsername :: T.Text }
    deriving (Eq, Hashable, Show, Read, Typeable, Ord)

data MUState innerState = MUState
    { _musUserStates    :: !(HMS.HashMap Username innerState)
    , _musActiveUser    :: !(Maybe Username)
    , _musUsernameField :: !T.Text
    } deriving (Show)

type MUAction innerAction = WithWindowActions (MUAction' innerAction)
data MUAction' innerAction
    = SignInA
    | SignOutA
    | UpdateUsernameFieldA T.Text
    | InnerA (WithWindowActions innerAction)
    deriving (Eq, Ord, Read, Show, Typeable)

makeLenses ''MUState

initialMUState :: MUState s
initialMUState = MUState
    { _musUserStates    = HMS.empty
    , _musActiveUser    = Nothing
    , _musUsernameField = ""
    }

applyMUAction
    :: (Eq s)
    => s
    -> ((WithWindowActions a) -> Transition s (WithWindowActions a))
    -> MUAction a
    -> Transition (MUState s) (MUAction a)
applyMUAction initialInnerState applyInnerAction action =
    runTransitionM $ case action of
      -- Pass path changes along to the inner app
      PathChangedTo path -> mkTransitionM $
        applyMUAction initialInnerState applyInnerAction $
          AppAction $ InnerA $ PathChangedTo path
      AppAction SignInA -> do
        username <- use musUsernameField
        musActiveUser .= case username of
          "" -> Nothing
          x  -> Just $ Username x
        musUsernameField .= ""
      AppAction SignOutA ->
        musActiveUser .= Nothing
      AppAction (UpdateUsernameFieldA text) ->
        musUsernameField .= text
      AppAction (InnerA innerAction) ->
        use musActiveUser >>= \case
          Nothing -> return ()
          Just userId ->
            -- NOTE (AS): The use of 'non' here introduces the (Eq s)
            -- constraint above. I don't think this is necessary.
            zoomTransition
              (AppAction . InnerA)
              (musUserStates . at userId . non initialInnerState)
              (mkTransitionM $ applyInnerAction innerAction)

renderMUState
    :: (Show a, Show s, Eq s)
    => s
    -> (s -> WindowState (WithWindowActions a))
    -> MUState s
    -> WindowState (MUAction a)
renderMUState initialInnerState renderInnerState state =
    case state ^. musActiveUser of
      Nothing -> WindowState
        { _wsPath = ""
        , _wsBody = renderNotSignedIn state
        }
      Just username ->
        let innerState = state ^. musUserStates . at username . non initialInnerState
            WindowState innerBody innerPath = renderInnerState innerState
        in WindowState
          { _wsPath = innerPath
          , _wsBody = renderSignedIn innerBody username
          }

renderSignedIn
    :: H.Html (WithWindowActions a)
    -> Username
    -> H.Html (MUAction a)
renderSignedIn innerBody username = do
    H.div H.! A.class_ "multiuser-bar" $ do
      H.span $ "Logged in as " <> H.toHtml (unUsername username) <> ". "
      H.span H.! E.onClick' (AppAction SignOutA) $ "Sign out"
    E.mapActions (AppAction . InnerA) innerBody

renderNotSignedIn :: MUState s -> H.Html (MUAction a)
renderNotSignedIn state =
    H.div H.! A.class_ "multiuser-signin" $ do
      H.p "Not logged in. Please specify username:"
      H.input H.! A.autofocus True
              H.! A.value (H.toValue $ view musUsernameField state)
              H.! E.onValueChange (AppAction . UpdateUsernameFieldA)
              H.! E.onKeyDown [Keycode.enter] (AppAction SignInA)

withMultiUser
    :: (Show a, Show s, Eq s)
    => App s (WithWindowActions a)
    -> App (MUState s) (MUAction a)
withMultiUser innerApp = App
    { appInitialState    = initialMUState
    , appInitialRequests = fmap (AppAction . InnerA) <$> appInitialRequests innerApp
    , appApplyAction     = applyMUAction (appInitialState innerApp) (appApplyAction innerApp)
    , appRender          = renderMUState (appInitialState innerApp) (appRender innerApp)
    }
