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
  , loadStaticFiles

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
import qualified Blaze.Development.Server.Assets   as Assets

import           Control.Arrow                ((&&&), second)
import           Control.Concurrent           (threadDelay)
import qualified Control.Concurrent.Async     as Async
import           Control.Concurrent.STM.TVar
                 ( TVar, newTVarIO, readTVar, writeTVar, readTVarIO
                 )
import           Control.Exception            (finally, evaluate, catch)
import           Control.Lens                 (preview, _Left)
import           Control.Monad
import           Control.Monad.Managed        (Managed, runManaged, managed)
import           Control.Monad.STM            (STM, atomically)
import           Control.Monad.State

import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                    as T

import qualified Network.Wai.Application.Static  as Static
import           Network.Wai.Handler.Warp        (run)
import           Numeric                         (showHex)

import           Servant
import           Servant.HTML.BlazeReact      (HTML)

import           System.Directory
                 ( renameFile, removeFile, doesFileExist, doesDirectoryExist
                 , getDirectoryContents
                 )
import           System.FilePath              ((</>), takeExtension)
import           System.IO
                 ( withBinaryFile, IOMode(ReadMode, WriteMode)
                 )
import           System.Random                (getStdRandom, randomR)

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
    , cStateFile :: !FilePath
      -- ^ Path to the file that should be used to persist the application
      -- state.
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
defaultConfig :: [T.Text] -> [(FilePath, B.ByteString)] -> [FilePath] -> Config
defaultConfig externalStylesheets staticFiles additionalScripts =
    Config
    { cPort                = 8081
    , cExternalServerUrl   = "http://localhost:8081"
    , cExternalStylesheets = externalStylesheets
    , cStaticFiles         = staticFiles
    , cStateFile           = "blaze-react-dev-mode_state.json"
    , cAdditionalScripts   = additionalScripts
    }


-- | Start a development server with the given configuation.
main :: (Aeson.ToJSON st, Aeson.FromJSON st) => Config -> HtmlApp st act -> IO ()
main config app = do
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
        -- create executable app instance
        h <- newHandle (cStateFile config) loggerH app

        -- announce that we serve the app over http
        liftIO $ Logger.logInfo loggerH $ unlines
          [ "Server started at: " <> T.unpack (cExternalServerUrl config)
          , "Press CTRL-C to stop it"
          ]

        -- actually serve the app
        liftIO $
          (run port $ serve api $ serveApi config h)
            `finally` Logger.logInfo loggerH "Server stopped."
  where
    port    = cPort config


-- | Start a server with a dummy HTML application. We use this for testing
-- purposes.
testMain :: IO ()
testMain =
    main (defaultConfig [bootstrapUrl] [] []) dummyHtmlApp
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
newHandle
    :: forall st act.
       (Aeson.ToJSON st, Aeson.FromJSON st)
    => FilePath -> Logger.Handle -> HtmlApp st act -> Managed Handle
newHandle stateFile loggerH (RenderableApp render app) = do
    -- allocate state reference
    stVar <- liftIO $ do
        -- try to read initial state, and fallback to app initial state otherwise
        errOrSt <- readFileAsJson stateFile
        case errOrSt of
          Left err -> do
            Logger.logInfo loggerH $ unlines
                [ "Failed to read state from '" <> stateFile <> "': "
                , err
                , "Falling back to initial application state at revision 0."
                ]
            newTVarIO (appInitialState app, 0)
          Right st -> do
            Logger.logInfo loggerH $
                "Using state at revision " <> show (snd st) <>
                " from '" <> stateFile <> "'."
            newTVarIO st

    -- start asynchronous state writer thread
    currentRevId <- liftIO $ snd <$> readTVarIO stVar
    void $ managed $ Async.withAsync $ persistStateOnChange stVar currentRevId

    -- execute the initial request
    liftIO $ runIORequest (appInitialRequest app) (applyActionIO stVar)

    -- return event handlers
    return $ Handle (handleGetView stVar) (handleHandleEvent stVar)
  where
    persistStateOnChange :: TVar (st, ProxyApi.RevisionId) -> ProxyApi.RevisionId -> IO ()
    persistStateOnChange stVar currentRevId = do
        st <- atomically $ do
            st@(_, revId) <- readTVar stVar
            guard (currentRevId /= revId)
            return st
        -- write file and persist state on a change
        writeFileAsJson stateFile st `catch` handleIOError
        persistStateOnChange stVar (snd st)
      where
        handleIOError :: IOError -> IO ()
        handleIOError err = Logger.logInfo loggerH $
            "Error when writing '" <> stateFile <> "': " <> show err

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

serveApi :: Config -> Handle -> Server Api
serveApi config h =
         (servePostEventApi :<|> serveViewApi)
    :<|> return (indexHtml config)
    :<|> return (T.pack ProxyApi.markdownDocs)
    :<|> return (serverUrlScript (cExternalServerUrl config))
    :<|> serveStaticFiles (Assets.staticFiles <> cStaticFiles config)
  where
    servePostEventApi ev      = liftIO $ hHandleEvent h ev
    serveViewApi mbKnownRevId = liftIO $ hGetView h mbKnownRevId

indexHtml :: Config -> H.Html ()
indexHtml config =
    -- TODO (SM): add doctype once it is supported by H.Html
    H.html $ mconcat
      [ H.head $ mconcat
         [ foldMap stylesheet (cExternalStylesheets config)
         , foldMap (stylesheet . ("static" </>)) staticStylesheets
         , H.link H.! A.rel "shortcut icon" H.! A.href "static/favicon.ico"
         , foldMap script_ ["js/rts.js", "js/lib.js", "js/out.js", "js/SERVER-URL.js"]
         ]
      , H.body mempty
      , foldMap (script_ . H.toValue) (cAdditionalScripts config)
        -- FIXME (SM): we should support for attributes without a value to the
        -- Html type.
      , script "js/runmain.js" H.! A.defer "defer" $ mempty
      ]
  where
    stylesheet url = H.link H.! A.rel "stylesheet" H.! A.href (H.toValue url)
    script  url = H.script H.! A.src ("static/" <> url)
    script_ url = script url mempty

    staticStylesheets =
        filter ((".css" ==) . takeExtension) (fst <$> cStaticFiles config)


serveStaticFiles :: [(FilePath, B.ByteString)] -> Server Raw
serveStaticFiles = Static.staticApp . Static.embeddedSettings

serverUrlScript :: T.Text -> T.Text
serverUrlScript externalApiUrl =
    "BLAZE_REACT_DEV_MODE_SERVER_URL = \"" <> externalApiUrl <> "\""



------------------------------------------------------------------------------
-- File storage
------------------------------------------------------------------------------

writeFileAsJson :: Aeson.ToJSON a => FilePath -> a -> IO ()
writeFileAsJson file = writeFileRaceFree file . Aeson.encode

readFileAsJson :: Aeson.FromJSON a => FilePath -> IO (Either String a)
readFileAsJson file =
    -- make sure that file is closed on exit
    ( withBinaryFile file ReadMode $ \h -> do
          content <- BL.hGetContents h
          -- force the lazy file reading here, as it will be closed as soon as
          -- we return from this do-block
          evaluate (Aeson.eitherDecode' content)
     ) `catch` handleIOError
  where
    handleIOError :: IOError -> IO (Either String a)
    handleIOError = return . Left . show


-- | A probalistically race-free version for writing a state file.
writeFileRaceFree :: FilePath -> BL.ByteString -> IO ()
writeFileRaceFree stateFile content = do
    -- compute random filename
    suffix <- getRandomHex32BitNumber
    let tempStateFile = stateFile <> "-" <> suffix
    -- overwrite in an atomic fashion
    writeAndMove tempStateFile `finally` tryRemoveFile tempStateFile
  where
    writeAndMove tempFile = do
        withBinaryFile tempFile WriteMode $ \h -> BL.hPut h content
        renameFile tempFile stateFile

    tryRemoveFile file = do
        doesExist <- doesFileExist file
        when doesExist (removeFile file)

    getRandomHex32BitNumber = do
        i <- getStdRandom (randomR (0x10000000::Int,0xffffffff))
        return $ showHex i ""

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
