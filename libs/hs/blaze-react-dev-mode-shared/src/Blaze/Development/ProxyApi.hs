{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module is intended to be imported qualified as follows.
--
-- > import qualified Blaze.Development.ProxyApi as ProxyApi
--
module Blaze.Development.ProxyApi
  ( SessionId(..)
  , RevisionId(..)
  , Position(..)
  , Event(..)
  , View(..)

  , PostEvent
  , GetView

  , Api
  , api

    -- * Documentation
  , markdownDocs

    -- * Testing support
  , clickEvSample
  ) where


import qualified Data.Aeson                   as Aeson
import           Data.Proxy                   (Proxy(..))
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TLE

import           GHC.Generics                 (Generic)

import           Servant.API
import           Servant.Docs

import qualified Text.Blaze.Event             as E
import qualified Text.Blaze.Event.Internal    as E
import qualified Text.Blaze.Html5             as H


------------------------------------------------------------------------------
-- API definition
------------------------------------------------------------------------------

-- | A session-id identifying a session of an application running on the
-- server.
newtype SessionId = SessionId { unSessionId :: Integer }
    deriving (Eq, Ord, Show, Num, Enum, Aeson.ToJSON, Aeson.FromJSON, FromText, ToText)

-- | A revision of an application state whose rendering is being proxied.
newtype RevisionId = RevisionId { unRevisionId :: Integer }
    deriving (Eq, Ord, Show, Num, Enum, Aeson.ToJSON, Aeson.FromJSON, FromText, ToText)

-- | A position in a traversable structure.
newtype Position = Position { unPosition :: Int }
    deriving (Eq, Ord, Show, Num, Enum, Aeson.ToJSON, Aeson.FromJSON, FromText, ToText)

-- | @Event revId pos someEv@ denotes the fact
-- that the concrete event @someEv@ was triggered from the event-handler at
-- position @pos@ in the rendered version of the state at revision @revId@.
data Event = Event !RevisionId !Position !E.SomeEvent
    deriving (Generic)

-- | @View revId html@ denotes the rendered view of the state at revision
-- @revId@ where all event-handlers are referended by position, which makes
-- the serializable.
data View = View !RevisionId !(H.Html (Position, E.SomeEventSelector))
    deriving (Generic)


-- instances
------------

instance Aeson.ToJSON   Event
instance Aeson.FromJSON Event

instance Aeson.ToJSON   View
instance Aeson.FromJSON View

showJson :: Aeson.ToJSON a => a -> String
showJson = TL.unpack . TLE.decodeUtf8 . Aeson.encode

instance Show Event where
    show = showJson

instance Show View where
    show = showJson


------------------------------------------------------------------------------
-- API definition
------------------------------------------------------------------------------

-- | Shared definition of the prefix to use for the API.
type WithApiPrefix a =
    "api" :> "proxy" :> Capture "sessionId" SessionId :> a

-- | The endpoint that allows to send an event that happened.
type PostEvent
    = WithApiPrefix ("event" :> ReqBody '[JSON] Event :> Post '[JSON] ())

-- | The endpoint for retrieving the rendered view of the
-- current state of the application. To allow for long-polling, this call will
-- block until the revision is different from the given known revision.
type GetView
    = WithApiPrefix
        ("view" :> QueryParam "knownRevision" RevisionId :> Get '[JSON] View)

-- | The API serving all endpoints necessary for proxying the HTML-based
-- interaction of a blaze-react app.
type Api = PostEvent :<|> GetView

-- | A 'Proxy' for our API.
api :: Proxy Api
api = Proxy


------------------------------------------------------------------------------
-- Documentation
------------------------------------------------------------------------------

-- | A markdown-based explanation of our API endpoints.
markdownDocs :: String
markdownDocs = markdown $ docs api





------------------------------------------------------------------------------
-- API documentation
------------------------------------------------------------------------------

clickEvSample :: E.SomeEvent
clickEvSample  =
    E.SomeEvent (E.Event (E.OnClick [E.LeftButton]) mousePos)
  where
    mousePos = E.MousePosition
      { E.mpClientX = 10
      , E.mpClientY = 20
      , E.mpPageX   = 100
      , E.mpPageY   = 200
      , E.mpScreenX = 1000
      , E.mpScreenY = 2000
      }

instance ToSample () () where
    toSample _ = Just ()

instance ToSample View View where
    toSample _ = Just (View 1 (H.h1 "there shall be Html!!!"))

instance ToSample Event Event where
    toSample _ = Just (Event 11 12 clickEvSample)

instance ToParam (QueryParam "knownRevision" RevisionId) where
  toParam _ =
      DocQueryParam "knownRevision"                     -- name
                    ["0", "1"] -- example of values (not necessarily exhaustive)
                    "The revision currently known to the client. \
                    \The endpoint will block until it can return a different revision."
                    Normal

instance ToParam (QueryParam "sessionId" SessionId) where
  toParam _ =
      DocQueryParam "sessionId"                     -- name
                    ["0", "1"] -- example of values (not necessarily exhaustive)
                    "The application instance being proxied. This is used to \
                    \support multiple instances of applications running on the \
                    \same development server."
                    Normal

instance ToCapture (Capture "sessionId" SessionId) where
  toCapture _ = DocCapture "sessionId" "Session identifier (number)"


