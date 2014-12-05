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
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           Data.Typeable       (Typeable)

import qualified Text.Blaze.Html5    as H
import qualified Text.Blaze.Html5.Attributes          as A
import qualified Text.Blaze.Keycode                   as Keycode


newtype Username = Username { unUsername :: T.Text }
    deriving (Eq, Hashable, Show, Read, Typeable, Ord)

data MUState innerState = MUState
    { _musUserStates  :: HMS.HashMap Username innerState
    , _musActiveUser  :: Maybe Username
    , _musUsernameField :: T.Text
    } deriving (Show)

data MUAction innerAction
    = SignInA
    | SignOutA
    | UpdateUsernameFieldA T.Text
    | InnerA innerAction
    deriving (Eq, Ord, Read, Show, Typeable)

makeLenses ''MUState

initialMUState :: MUState s
initialMUState = MUState
    { _musUserStates  = HMS.empty
    , _musActiveUser  = Nothing
    , _musUsernameField = ""
    }

applyMUAction
    :: (Eq s)
    => s
    -> (a -> Transition s a)
    -> MUAction a
    -> Transition (MUState s) (MUAction a)
applyMUAction initialInnerState applyInnerAction action =
    runTransitionM $ case action of
      SignInA -> do
        username <- use musUsernameField
        musActiveUser .= case username of
          "" -> Nothing
          x  -> Just $ Username x
        musUsernameField .= ""
      SignOutA ->
        musActiveUser .= Nothing
      UpdateUsernameFieldA text ->
        musUsernameField .= text
      InnerA innerAction ->
        use musActiveUser >>= \case
          Nothing -> return ()
          Just userId ->
            -- NOTE (AS): The use of 'non' here introduces the (Eq s)
            -- constraint above. I don't think this is necessary.
            zoomTransition InnerA (musUserStates . at userId . non initialInnerState) $
              mkTransitionM (applyInnerAction innerAction)


renderMUState
    :: (Show a, Show s)
    => s
    -> (s -> H.Html a)
    -> MUState s
    -> H.Html (MUAction a)
renderMUState initialInnerState renderInnerState state =
    case state ^. musActiveUser of
      Just username -> do
        let innerState = fromMaybe initialInnerState $
              state ^. musUserStates . at username
            innerHtml = H.mapActions InnerA $ renderInnerState innerState
        H.div H.! A.class_ "multiuser-bar" $ do
          H.span $ "Logged in as " <> H.toHtml (unUsername username) <> ". "
          H.span H.! H.onClick SignOutA $ "Sign out"
        innerHtml
      Nothing ->
        H.div H.! A.class_ "multiuser-signin" $ do
          H.p "Not logged in. Please specify username:"
          H.input H.! A.autofocus True
                  H.! A.value (H.toValue $ view musUsernameField state)
                  H.! H.onTextInputChange UpdateUsernameFieldA
                  H.! H.onKeyPress Keycode.enter SignInA

withMultiUser
    :: (Show a, Show s, Eq s)
    => App s a
    -> App (MUState s) (MUAction a)
withMultiUser innerApp = App
    { appInitialState    = initialMUState
    , appInitialRequests = fmap InnerA <$> appInitialRequests innerApp
    , appApplyAction     = applyMUAction (appInitialState innerApp) (appApplyAction innerApp)
    , appRender          = renderMUState (appInitialState innerApp) (appRender innerApp)
    }
