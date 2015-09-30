{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | An app that allows to play the client for an arbitrary other app running
-- on the development-server. This module is intended to be imported
-- qualified.
module Blaze.Development.Client
  ( Action(..)
  , Request(..)
  , State(..)
  , app
  , render

    -- * IO-based interface
  , Handle(..)
  , clientAppFor
  ) where


import           Blaze.Core
import qualified Blaze.Development.ProxyApi as ProxyApi
import qualified Blaze.Development.Internal.Logger        as Logger
import           Blaze.Development.Internal.Types
                 ( RenderableApp(..), HtmlApp
                 )

import           Control.Applicative
import           Control.Concurrent           (threadDelay)
import           Control.Lens
                 ( (<%=), (.=), (^.), makeLenses
                 )
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Either   (EitherT, runEitherT)

import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Time.Clock
                 ( UTCTime, addUTCTime, diffUTCTime, getCurrentTime )

import           Prelude                    hiding (lookup)

import qualified Text.Blaze.Event           as E
import qualified Text.Blaze.Event.Internal  as EI
import qualified Text.Blaze.Html5           as H



------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type RevId = ProxyApi.RevisionId
type Pos   = ProxyApi.Position

-- | Client state encapsulating server-state 'ss' and view 'v'.
type ViewState = Maybe (RevId, H.Html (Pos, E.SomeEventSelector))

data State = State
    { _sViewState         :: !ViewState
    , _sNumFailedAttempts :: !Int
    , _sServerName        :: !T.Text
    }

-- | Actions that happen in the mirror application.
data Action
    = HandleGetReflection !UTCTime !(Maybe ProxyApi.View)
      -- ^ Current time, and the result of fetching a new view, which might be
      -- 'Nothing' in case of a failure.
    | HandleEventA !ProxyApi.Event
      -- ^ Handle the event that was triggered in our view, which is the
      -- rendered version of the state with the given revision-id.
    deriving (Show)

data Request
    = GetReflectionR !(Maybe UTCTime) !(Maybe RevId)
      -- ^ Request the next mirror-image after the given time, which must have
      -- a revision-id that is larger than the one that we are currently
      -- displaying.
    | HandleEventR !ProxyApi.Event
    deriving (Show)


-- lenses
---------

makeLenses ''State


------------------------------------------------------------------------------
-- Mirror serving and rendering
------------------------------------------------------------------------------

render :: State -> H.Html (E.EventHandler Action)
render st = case _sViewState st of
    Nothing            -> "loading..."
    Just (revId, html) ->
      (if _sNumFailedAttempts st == 0 then mempty else pollingMsg) <>
      (toAction revId <$> html)
  where
    pollingMsg =
        "Could not reach '" <> H.toHtml (st ^. sServerName) <> "': polling..."

    toAction revId (pos, EI.SomeEventSelector sel) =
        EI.EventHandler sel $ \evData -> HandleEventA $
            ProxyApi.Event revId pos (EI.SomeEvent (EI.Event sel evData))


-- | Create an app for proxying the session with the given 'SessionId', which
-- is usually chosen randomly.
--
-- We assume that each request for a next server state will be answered with
-- either 'Nothing' or a new server-state.
--
-- TODO (SM): this is more like mirroring a session over an unreliable channel
-- => consider a rename.
app :: T.Text -> App State Action [Request]
app serverName = App
    { appInitialState   = State
        { _sViewState         = Nothing
        , _sNumFailedAttempts = 0
        , _sServerName        = serverName
        }
    , appInitialRequest = [GetReflectionR Nothing Nothing]
    , appApplyAction    = \act -> runApplyActionM $ do
        case act of
          HandleEventA ev ->
              submitRequest [HandleEventR ev]
          HandleGetReflection now mbView ->
            case mbView of
              Just (ProxyApi.View revId html) -> do
                sViewState         .= Just (revId, html)
                sNumFailedAttempts .= 0
                submitRequest [GetReflectionR (Just now) (Just revId)]
              Nothing -> do
                numFailedAttempts <- sNumFailedAttempts <%= succ
                let backoff   = min 1 (0.1 * (1.05 ^^ numFailedAttempts))
                    nextFetch = backoff `addUTCTime` now
                submitRequest [GetReflectionR (Just nextFetch) Nothing]
    }


------------------------------------------------------------------------------
-- IO Based handling of the requests
------------------------------------------------------------------------------

-- | The monad for executing a client request.
type ClientM = EitherT String IO

-- | A 'Handle' for a remote proxy-API server and logging support
data Handle = Handle
    { hLogger    :: !Logger.Handle
    , hPostEvent :: !(ProxyApi.Event -> ClientM ())
    , hGetView   :: !(Maybe ProxyApi.RevisionId -> ClientM ProxyApi.View)
    }


clientAppFor :: Handle -> T.Text -> HtmlApp State Action
clientAppFor h serverName = RenderableApp
    { raApp    = fmap (foldMap runClientRequest) (app serverName)
    , raRender = render
    }
  where
    runClientRequest :: Request -> IORequest Action
    runClientRequest = \case
        -- FIXME (SM): 'concurrently' leads to unintended behaviour here, as it
        -- will fork an independent thread of execution that is not torn down
        -- when the 'main' function finishes. We'll need to use Async with
        -- linking in the IORequest type to ensure that we do not leak
        -- resources in IORequests.
        GetReflectionR mbAt mbCurrentRevId -> concurrently $ do
            errOrView <- liftIO $ do
                -- block until the requested time
                case mbAt of
                  Nothing -> return ()
                  Just at -> do
                    beforeReq <- getCurrentTime
                    let delayMicros = floor (at `diffUTCTime` beforeReq * 1e6)
                    unless (delayMicros <= 0) (threadDelay delayMicros)
                -- execute the request
                runEitherT $ hGetView h mbCurrentRevId

            afterReq <- liftIO $ getCurrentTime
            case errOrView of
              Right view -> return $ HandleGetReflection afterReq (Just view)
              Left err   -> do
                liftIO $ logError "getView" err
                return $ HandleGetReflection afterReq Nothing

        HandleEventR ev -> concurrently $ performIO_ $ do
            errOrRes <- runEitherT $ hPostEvent h ev
            case errOrRes of
              Left err -> logError "postEvent" err
              Right () -> return ()
      where
        logError ctxt err =
            Logger.logInfo (hLogger h) $ ctxt <> ": " <> show err
