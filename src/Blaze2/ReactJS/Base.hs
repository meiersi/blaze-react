{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}

module Blaze2.ReactJS.Base
    ( WindowState(..), wsBody, wsPath
    , WindowAction(..)

    , ignoreWindowActions
    , passWindowActionsThrough
    ) where

import           Blaze2.Core

import           Control.Lens          (makeLenses)

import           Data.Monoid           (Monoid, mempty)
import qualified Data.Text             as T
import           Data.Typeable         (Typeable)

import           Prelude               hiding (div)

import qualified Text.Blaze.Html5      as H


data WindowState act = WindowState
    { _wsBody  :: !(H.Html act)
    , _wsPath  :: !T.Text
      -- TODO (asayers): _wsTitle :: T.Text
    }

data WindowAction
    = PathChangedTo !T.Text
    deriving (Show, Typeable, Eq, Ord, Read)

makeLenses ''WindowState


ignoreWindowActions
    :: Monoid req => App st act req -> App st (Either WindowAction act) req
ignoreWindowActions app = app
    { appApplyAction = \action -> case action of
        Left (PathChangedTo _) -> \state -> (state, mempty)
        Right innerAction      -> appApplyAction app innerAction
    }

passWindowActionsThrough
   :: (forall x. x -> actO x)
   -> App st (actO (Either WindowAction actI)) req
   -> App st (Either WindowAction (actO (Either WindowAction actI))) req
passWindowActionsThrough wrapAction (App initSt initReq apply) = App
    { appInitialState   = initSt
    , appInitialRequest = initReq
    , appApplyAction    = \act -> case act of
          Left wa -> apply (wrapAction $ Left wa)
          Right x -> apply x
    }
