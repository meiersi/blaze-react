{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | A HTTP server that serves a blaze-react app to a proxy running in a
-- browser.
module Blaze.Development.Server
  (
    -- * Applications
    RenderableApp(..)
  , HtmlApp

    -- * Starting a development server
  , Config(..)
  , defaultConfig

  , main

    -- * Testing
  , testMain
  ) where

import           Blaze.Core
import qualified Blaze.Development.Internal.Logger as Logger
import           Blaze.Development.Internal.Types
                 ( RenderableApp(..), HtmlApp, dummyHtmlApp
                 )
import qualified Blaze.Development.ProxyApi        as ProxyApi

import           Control.Concurrent           (threadDelay)
import qualified Control.Concurrent.Async     as Async
import           Control.Concurrent.STM.TVar  (TVar, newTVarIO, readTVar, writeTVar)
import           Control.Exception            (finally)
import           Control.Lens                 (preview, _Left)
import           Control.Monad
import           Control.Monad.STM            (STM, atomically)
import           Control.Monad.State

import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                    as T

import           Network.Wai.Handler.Warp     (run)

import           Servant
import           Servant.HTML.BlazeReact      (HTML)

import           System.Directory             (getCurrentDirectory)
import           System.FilePath              ((</>))

import qualified Text.Blaze.Event.Internal    as EI
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A


------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

-- | A URL of an external stylesheet.
type StylesheetUrl = T.Text

data Config = Config
    { cPort :: !Int
      -- ^ The port that we should serve from.
    , cStaticFilesDir :: !(Maybe FilePath)
      -- ^ The directory containing the static files for the proxy. The
      -- built-in files are served if 'Nothing' is given.
    , cExternalServerUrl :: !T.Text
      -- ^ The URL under which our server will be reachable by the client.
    , cExternalStylesheets :: ![StylesheetUrl]
      -- ^ A list of URL's pointing to external stylesheets that should be
      -- loaded as well.
    } deriving (Eq, Show)


------------------------------------------------------------------------------
-- Extended API definition
------------------------------------------------------------------------------

-- | Fetch the dynamically generated 'index.html' page.
type GetIndexHtml = Get '[HTML] (H.Html ())

type Api
    =    ProxyApi.Api
    :<|> GetIndexHtml
    :<|> ("api" :> "proxy" :> Get '[PlainText] T.Text)
    :<|> ("static" :> "js" :> "SERVER-URL.js" :> Get '[PlainText] T.Text)
    :<|> ("static" :> Raw)

api :: Proxy Api
api = Proxy


------------------------------------------------------------------------------
-- 'main' functions
------------------------------------------------------------------------------

-- | The defualt configuration starting the server on port 8081 serving the
-- given external stylesheets.
defaultConfig :: [T.Text] -> Config
defaultConfig externalStylesheets =
    Config
    { cPort                = 8081
    , cExternalServerUrl   = "http://localhost:8081"
    , cExternalStylesheets = externalStylesheets
    , cStaticFilesDir      = Nothing
    }

-- | Start a development server with the given configuation.
main :: Config -> HtmlApp st act -> IO ()
main config app = do
    -- allocate logger
    loggerH <- Logger.newStdoutLogger

    -- get static file serving directory
    staticFilesDir <- case cStaticFilesDir config of
        Just staticFilesDir -> return staticFilesDir
        Nothing             -> do
          pwd <- getCurrentDirectory
          return $! pwd </> "libs/hs/blaze-react-dev-mode-server/static"

    -- create executable app instance
    h <- newHandle loggerH app

    -- announce that we serve the app over http
    Logger.logInfo loggerH $ unlines
      [ "Server started at: " <> T.unpack (cExternalServerUrl config)
      , "Press CTRL-C to stop it"
      ]

    -- actually serve the app
    ( run port $ serve api $
          serveApi staticFilesDir
                   (cExternalServerUrl config)
                   (cExternalStylesheets config)
                   h
     ) `finally` Logger.logInfo loggerH "Server stopped."
  where
    port    = cPort config


-- | Start a server with a dummy HTML application. We use this for testing
-- purposes.
testMain :: IO ()
testMain =
    main (defaultConfig [bootstrapUrl]) dummyHtmlApp
  where
    bootstrapUrl =
       "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"


------------------------------------------------------------------------------
-- Application execution
------------------------------------------------------------------------------

-- | A 'Handle' for a service that runs a single instance of an application,
-- and provides support for proxying its display to a remote browser.
data Handle = Handle
    { hGetView :: !(Maybe ProxyApi.RevisionId -> IO ProxyApi.View)
      -- ^ @hGetView h mbRevId@ returns the rendered state of the application
      -- provided the given revision is older than the application's state.
    , hHandleEvent :: !(ProxyApi.Event -> IO ())
      -- @hHandleEvent@ allows applying an event to a specific revision of the
      -- application state.
    }

-- | Create a new handle that allows running with a single instance of an
-- HtmlApp.
newHandle :: forall st act. Logger.Handle -> HtmlApp st act -> IO Handle
newHandle loggerH (RenderableApp render app) = do
    -- allocate state reference
    stVar <- newTVarIO (appInitialState app, 0)

    -- execute the initial request
    runIORequest (appInitialRequest app) (applyActionIO stVar)

    -- return event handlers
    return $ Handle (handleGetView stVar) (handleHandleEvent stVar)
  where
    applyAction :: TVar (st, ProxyApi.RevisionId) -> act -> STM (IORequest act)
    applyAction stVar act = do
        (st, revId) <- readTVar stVar
        let (!st', !req) = appApplyAction app act st
        writeTVar stVar (st', succ revId)
        return req

    applyActionIO :: TVar (st, ProxyApi.RevisionId) -> act -> IO ()
    applyActionIO stVar act = do
        req <- atomically (applyAction stVar act)
        runIORequest req (applyActionIO stVar)

    -- function to handle a 'HandleEvent' mirror-request
    handleHandleEvent stVar (ProxyApi.Event evRevId pos someEv) = do
        req <- atomically $ do
            (st, revId) <- readTVar stVar
            if revId /= evRevId
              then logInfo' "event does not match revision-id"
              else
                case lookupByPosition pos (render st) of
                  Nothing -> logInfo' "ignore event that we cannot locate"
                  Just (EI.EventHandler sel evDataToAct) ->
                    case EI.someEventData someEv sel of
                      Nothing     -> logInfo' "event selectors do not match"
                      Just evData -> applyAction stVar (evDataToAct evData)

        -- execute resulting request (including the log-messages)
        runIORequest req (applyActionIO stVar)
      where
        logInfo' msg = return $ performIO_ (Logger.logInfo loggerH msg)

    -- function to handle a 'GetView' mirror-request
    handleGetView stVar mbClientRevId =
        -- TODO (SM): replace 20s teimout with a configurable one and do not
        -- return full data
        Async.withAsync (threadDelay (20 * 1000000)) $ \timeout ->
            atomically $ do
                (st, revId) <- readTVar stVar
                timedOut <- isJust <$> Async.pollSTM timeout
                -- only return an update after a timeout or when the revision-id
                -- has changed
                guard (timedOut || mbClientRevId /= Just revId)
                return $ ProxyApi.View revId  (traverseWithPosition adapt (render st))
      where
        adapt pos (EI.EventHandler sel _mkAct) = (pos, EI.SomeEventSelector sel)


-- | Lookup the element at the given position.
--
-- TODO (SM): this code could be shortened using the fact that 'traversed'
-- provides and IndexeTraversal.
lookupByPosition
    :: Traversable f => ProxyApi.Position -> f a -> Maybe a
lookupByPosition i t =
    preview _Left $ execStateT (traverse lookup' t) 0
  where
    lookup' x = do
        nextId <- get
        put (succ nextId)
        when (nextId == i) (lift (Left x))


-- TODO (SM): this code could be shortened using the fact that 'traversed'
-- from 'lens' provides an IndexedTraversal.
traverseWithPosition
    :: Traversable f => (ProxyApi.Position -> a -> b) -> f a -> f b
traverseWithPosition f t =
    evalState (traverse annotate t) 0
  where
    annotate x = do
        nextId <- get
        let !nextId' = succ nextId
        put nextId'
        return (f nextId x)


------------------------------------------------------------------------------
-- API serving
------------------------------------------------------------------------------

serveApi :: FilePath -> T.Text -> [StylesheetUrl] -> Handle -> Server Api
serveApi staticFilesDir externalServerUrl stylesheetUrls h =
         (servePostEventApi :<|> serveViewApi)
    :<|> return (indexHtml stylesheetUrls)
    :<|> return (T.pack ProxyApi.markdownDocs)
    :<|> return (serverUrlScript externalServerUrl)
    -- FIXME (SM): will need to embed these files into library for
    -- location-independent-deployment; or make this directory configurable.
    :<|> serveDirectory staticFilesDir
  where
    servePostEventApi ev      = liftIO $ hHandleEvent h ev
    serveViewApi mbKnownRevId = liftIO $ hGetView h mbKnownRevId


indexHtml :: [T.Text] -> H.Html ()
indexHtml stylesheetUrls =
    -- FIXME (SM): add doctype once it is supported by H.Html
    H.html
      ( H.head
         ( foldMap (stylesheet . H.toValue) stylesheetUrls <>
           H.link H.! A.rel "shortcut icon" H.! A.href "static/favicon.ico" <>
           foldMap script_ ["rts.js", "lib.js", "out.js", "SERVER-URL.js"]
         ) <>
        H.body mempty <>
        -- FIXME (SM): we should support for attributes without a value.
        (script "runmain.js" H.! A.defer "defer" $ mempty)
      )
  where
    stylesheet url = H.link H.! A.rel "stylesheet" H.! A.href url
    script  url = H.script H.! A.src ("static/js/" <> url)
    script_ url = script url mempty


serverUrlScript :: T.Text -> T.Text
serverUrlScript externalApiUrl =
    "BLAZE_REACT_DEV_MODE_SERVER_URL = \"" <> externalApiUrl <> "\""


