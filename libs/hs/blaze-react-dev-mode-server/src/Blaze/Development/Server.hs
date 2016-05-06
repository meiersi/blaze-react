{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A HTTP server that serves a blaze-react app to a proxy running in a
-- browser.
module Blaze.Development.Server
  (
    -- * Applications
    RenderableApp(..)
  , HtmlApp

    -- * JSON encodings
  , JsonEncoding
  , noJsonEncoding
  , fullJsonEncoding
  , projectionJsonEncoding

    -- * Starting a development server
  , Config(..)
  , defaultConfig
  , loadStaticFiles

  , main

    -- * Testing
  , testMain
  ) where


import qualified Blaze.Development.Internal.Logger as Logger
import           Blaze.Development.Internal.Types
                 ( RenderableApp(..), HtmlApp, dummyHtmlApp
                 )
import qualified Blaze.Development.ProxyApi               as ProxyApi
import qualified Blaze.Development.Server.Assets          as Assets
import qualified Blaze.Development.Server.ResourceManager as ResourceManager
import qualified Blaze.Development.Server.Session         as Session

import           Control.Arrow                ((&&&), second)
import qualified Control.Concurrent.Async     as Async
import           Control.Concurrent.MVar
                 ( MVar, newMVar, modifyMVar
                 )
import           Control.Exception            (finally, bracket)
import           Control.Lens
                 ( preview, makeLenses, ix, at, set, over, view
                 )
import           Control.Monad
import           Control.Monad.Managed        (runManaged, managed)
import           Control.Monad.STM            (atomically)
import           Control.Monad.State
import           Control.Monad.Trans.Either   (EitherT, left)
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Types             as Aeson
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC8
import qualified Data.Map.Strict              as MS
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                    as T

import qualified Network.Wai.Application.Static  as Static
import           Network.Wai.Handler.Warp        (run)

import           Servant
import           Servant.HTML.BlazeReact      (HTML)

import           System.Directory
                 ( doesFileExist, doesDirectoryExist
                 , getDirectoryContents
                 )
import           System.FilePath              ((</>), takeExtension, addExtension)

import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A


------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

-- | A URL of an external stylesheet.
type StylesheetUrl = T.Text

-- | A JSON encoding of a Haskell value.
type JsonEncoding a = (a -> Aeson.Value, Aeson.Value -> Aeson.Parser a)

data Config = Config
    { cPort :: !Int
      -- ^ The port that we should serve from.
    -- , cStaticFilesDir :: !(Maybe FilePath)
    --   -- ^ The directory containing the static files for the proxy. The
    --   -- built-in files are served if 'Nothing' is given.
    , cExternalServerUrl :: !T.Text
      -- ^ The URL under which our server will be reachable by the client.
    , cExternalStylesheets :: ![StylesheetUrl]
      -- ^ A list of URL's pointing to stylesheets that should be
      -- loaded from an external server.
    , cStaticFiles :: ![(FilePath, B.ByteString)]
      -- ^ A list of pairs of the name and the content of additional
      -- static files that should be served from '/static'.
    , cAdditionalScripts :: ![FilePath]
      -- ^ A list of scripts that should be loaded at the end of the page.
    , cStateDir :: !(Maybe FilePath)
      -- ^ Path to the directory that should be used to persist the state of
      -- the individual application sessions, if at all.
    } deriving (Eq, Show)

-- | State management for the concurrent sessions being run.
data ServerState = ServerState
    { _ssNextSessionId   :: !ProxyApi.SessionId
      -- ^ The next-session-id to use.
    , _ssSessions        :: !(MS.Map ProxyApi.SessionId Session.Handle)
      -- ^ Handle's for all sessions that are already running.
    }

-- | Server handle.
data Handle = Handle
    { hConfig          :: !Config
    , hStateVar        :: !(MVar ServerState)
    , hCreateSession   :: !(ProxyApi.SessionId -> IO Session.Handle)
      -- ^ How to create a new session of the application that we are serving.
    , hResourceManager :: !ResourceManager.Handle
      -- ^ The resource manager that we use to track the resources allocated
      -- by the individual sessions.
    }


-- optics
---------

makeLenses ''ServerState


------------------------------------------------------------------------------
-- Extended API definition
------------------------------------------------------------------------------

-- | Fetch a dynamically generated HTML page.
type GetHtmlPage = Get '[HTML] (H.Html ())

-- | The parameter for session-id specific paths.
type SessionIdParam = Capture "sessionId" ProxyApi.SessionId

-- | The whole API that we are serving.
type Api
    =    ProxyApi.Api
    :<|> GetHtmlPage
    :<|> ("s" :> SessionIdParam :> GetHtmlPage)
    :<|> ("s" :> SessionIdParam :> "SERVER-INFO.js" :> Get '[PlainText] T.Text)
    :<|> ("api" :> "proxy" :> Get '[PlainText] T.Text)
    :<|> ("static" :> Raw)

api :: Proxy Api
api = Proxy


------------------------------------------------------------------------------
-- 'main' functions
------------------------------------------------------------------------------

-- | The defualt configuration starting the server on port 8081 serving the
-- given external stylesheets.
defaultConfig :: [T.Text] -> [(FilePath, B.ByteString)] -> [FilePath] -> Config
defaultConfig externalStylesheets staticFiles additionalScripts =
    Config
    { cPort                = 8081
      -- FIXME (SM): we should not duplicate port and URL here. That is going
      -- to break setting the port number.
    , cExternalServerUrl   = "http://localhost:8081"
    , cExternalStylesheets = externalStylesheets
    , cStaticFiles         = staticFiles
    , cStateDir            = Just "blaze-react-dev-mode_state"
    , cAdditionalScripts   = additionalScripts
    }


-- | A JSON encoding that always returns the same constant.
noJsonEncoding :: JsonEncoding st
noJsonEncoding =
    (const (Aeson.toJSON ()), const (fail "Nothing was serialized."))

-- | A JSON encoding that encodes the complete state.
fullJsonEncoding :: (Aeson.ToJSON st, Aeson.FromJSON st) => JsonEncoding st
fullJsonEncoding = (Aeson.toJSON, Aeson.parseJSON)

-- | A JSON encoding that only encodes a projection of the state.
--
-- Use this encoding to ignore caches in the state.
--
projectionJsonEncoding
    :: (Aeson.ToJSON subSt, Aeson.FromJSON subSt)
    => (st -> subSt)
    -> (subSt -> st)
    -> JsonEncoding st
projectionJsonEncoding forget reconstruct =
    (Aeson.toJSON . forget, fmap reconstruct . Aeson.parseJSON)


-- | Start a development server with the given configuation.
main
    :: JsonEncoding st
       -- ^ The JSON encoding to persist (parts of) the application state.
       -- Use 'noJsonEncoding' if you don't want to persist anything.
    -> Config
    -> HtmlApp st act -> IO ()
main jsonEncoding config app = do
    -- allocate logger
    loggerH <- Logger.newStdoutLogger

    -- -- get static file serving directory
    -- staticFilesDir <- case cStaticFilesDir config of
    --     Just staticFilesDir -> return staticFilesDir
    --     Nothing             -> do
    --       pwd <- getCurrentDirectory
    --       return $! pwd </> "libs/hs/blaze-react-dev-mode-server/static"

    -- destroy all allocated resources when exiting here
    runManaged $ do
        -- announce that we serve the app over http
        liftIO $ Logger.logInfo loggerH $ unlines
          [ "Server started at: " <> T.unpack (cExternalServerUrl config)
          , "Press CTRL-C to stop it"
          ]

        -- create the resource manager, which we use to manage resources bound
        -- to new application sessions.
        --
        -- TODO (SM): Move this into its own module.
        --
        resourceManagerState <- managed $
            bracket Resource.createInternalState Resource.closeInternalState

        let resourceManagerH = ResourceManager.Handle
              { ResourceManager.allocate = \create destroy ->
                    flip Resource.runInternalState resourceManagerState $ do
                        (key, resource) <- Resource.allocate create destroy
                        return
                          ( resource
                          , ResourceManager.ReleaseKey (Resource.release key)
                          )
              }

        -- create server state
        serverStateVar <- liftIO $ newMVar $ ServerState
            { _ssNextSessionId   = 0
            , _ssSessions        = mempty
            }

        -- create server-handle
        let serverH = Handle
              { hConfig          = config
              , hStateVar        = serverStateVar
              , hResourceManager = resourceManagerH
              , hCreateSession   = createAppSession loggerH resourceManagerH
              }

        -- run the server in an async coupled to the lifteime of the main
        -- thread
        serverStatusVar <- managed $ Async.withAsync $
            (run port $ serve api $ serveApi serverH)

        -- wait until the server terminated or should be stopped
        liftIO $ do
          ( do atomically $ do
                   serverStatus <- Async.pollSTM serverStatusVar
                   guard (isJust serverStatus)
           ) `finally` (Logger.logInfo loggerH "Server stopped.")
  where
    port = cPort config

    createAppSession loggerH resourceManagerH sid =
        Session.newHandle jsonEncoding sessionConfig loggerH resourceManagerH app
      where
        sessionConfig = Session.Config mbStateFile
        mbStateFile = do
            stateDir <- cStateDir config
            let sessionName = "session-" <> show (ProxyApi.unSessionId sid)
            return $ stateDir </>  sessionName `addExtension` "json"


-- | Start a server with a dummy HTML application. We use this for testing
-- purposes.
testMain :: IO ()
testMain =
    main fullJsonEncoding (defaultConfig [bootstrapUrl] [] []) dummyHtmlApp
  where
    bootstrapUrl =
       "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"


------------------------------------------------------------------------------
-- API serving
------------------------------------------------------------------------------

type ServerM = EitherT ServantErr IO

serveProxyApi :: Handle -> Server ProxyApi.Api
serveProxyApi serverH = do
    servePostEventApi :<|> serveViewApi
  where
    -- ensure that the session exists, create it otherwise
    servePostEventApi :: Server ProxyApi.PostEvent
        -- :: Maybe ProxyApi.SessionId -> ProxyApi.Event -> ServerM ProxyApi.PostEvent
    servePostEventApi sid ev =
        withSession sid $ \h -> Session.hHandleEvent h ev

    serveViewApi :: Server ProxyApi.GetView
    serveViewApi sid mbKnownRevision =
        withSession sid $ \h -> Session.hGetView h mbKnownRevision

    withSession :: ProxyApi.SessionId -> (Session.Handle -> IO a) -> ServerM a
    withSession sid performAction = liftIO $ do
        -- ensure that a Session.Handle exists for the given session-id
        sessionH <- modifyMVar (hStateVar serverH) $ \st ->
            case preview (ssSessions . ix sid) st of
              Just sessionH  -> return (st, sessionH)
              Nothing -> do
                sessionH <- hCreateSession serverH sid
                return
                  ( set (ssSessions . at sid) (Just sessionH) st
                  , sessionH
                  )
        -- handle the action
        performAction sessionH


-- | Serve the whole API of the development server.
serveApi :: Handle -> Server Api
serveApi serverH =
         serveProxyApi serverH
    :<|> redirectToNewSession serverH
    :<|> serveSessionIndexHtml serverH
    :<|> serveServerInfoScript serverH
    :<|> return (T.pack ProxyApi.markdownDocs)
    :<|> serveStaticFiles serverH
  where

redirectToNewSession :: Handle -> ServerM (H.Html ())
redirectToNewSession serverH = do
    -- allocate new session-id
    sid <- liftIO $ modifyMVar (hStateVar serverH) $ \st ->
        pure (over ssNextSessionId succ st, view ssNextSessionId st)
    -- redirect to session specific URL
    let url = BC8.pack $ "s/" <> show (ProxyApi.unSessionId sid)
    left $ err303 { errHeaders = [("Location", url)] }


serveSessionIndexHtml :: Handle -> ProxyApi.SessionId -> ServerM (H.Html ())
serveSessionIndexHtml serverH sid =
    return $ indexHtml (hConfig serverH) sid

serveServerInfoScript :: Handle -> ProxyApi.SessionId -> ServerM T.Text
serveServerInfoScript serverH sid0 =
    return $ T.unlines
      [ "BLAZE_REACT_DEV_MODE_SERVER_URL = \"" <> externalApiUrl <> "\""
      , "BLAZE_REACT_DEV_MODE_SESSION_ID = \"" <> sid <> "\""
      ]
  where
    sid = T.pack $ show $ ProxyApi.unSessionId sid0
    externalApiUrl = cExternalServerUrl $ hConfig serverH

indexHtml :: Config -> ProxyApi.SessionId -> H.Html ()
indexHtml config sid =
    -- TODO (SM): add doctype once it is supported by H.Html
    H.html $ mconcat
      [ H.head $ mconcat
         [ foldMap stylesheet (cExternalStylesheets config)
         , foldMap (stylesheet . ("/static" </>)) staticStylesheets
         , H.link H.! A.rel "shortcut icon" H.! A.href "static/favicon.ico"
         , foldMap script_
             [ "/static/js/rts.js"
             , "/static/js/lib.js"
             , "/static/js/out.js"
             , "/s/" <> H.toValue (ProxyApi.unSessionId sid) <> "/SERVER-INFO.js"
             ]
         ]
      , H.body mempty
      , foldMap (script_ . H.toValue) (cAdditionalScripts config)
        -- FIXME (SM): we should support for attributes without a value to the
        -- Html type.
      , script "/static/js/runmain.js" H.! A.defer "defer" $ mempty
      ]
  where
    stylesheet url = H.link H.! A.rel "stylesheet" H.! A.href (H.toValue url)
    script  url = H.script H.! A.src url
    script_ url = script url mempty

    staticStylesheets =
        filter ((".css" ==) . takeExtension) (fst <$> cStaticFiles config)


serveStaticFiles :: Handle -> Server Raw
serveStaticFiles serverH =
    Static.staticApp $ Static.embeddedSettings $
        Assets.staticFiles <> cStaticFiles (hConfig serverH)




------------------------------------------------------------------------------
-- Static Assets
------------------------------------------------------------------------------

-- | Load the additional static files from a local directory.
loadStaticFiles :: FilePath -> IO [(FilePath, B.ByteString)]
loadStaticFiles topDir =
    -- Code copied from
    -- <http://hackage.haskell.org/package/file-embed-0.0.9/docs/src/Data-FileEmbed.html#embedFile>
    fileList' topDir ""
  where
    notHidden :: FilePath -> Bool
    notHidden ('.':_) = False
    notHidden _       = True

    liftPair2 :: Monad m => (a, m b) -> m (a, b)
    liftPair2 (a, b) = b >>= \b' -> return (a, b')

    fileList' :: FilePath -> FilePath -> IO [(FilePath, B.ByteString)]
    fileList' realTop top = do
        allContents <- filter notHidden <$> getDirectoryContents (realTop </> top)
        let all' = map ((top </>) &&& (\x -> realTop </> top </> x)) allContents
        files <- filterM (doesFileExist . snd) all' >>=
                 mapM (liftPair2 . second B.readFile)
        dirs <- filterM (doesDirectoryExist . snd) all' >>=
                mapM (fileList' realTop . fst)
        return $ concat $ files : dirs
