
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | The support for running individual sessions of blaze-react applications.
--
-- This module is intended to be imported qualified as follows.
--
-- > import qualified Blaze.Development.Server.Session as Session
--
module Blaze.Development.Server.Session
  (
    -- * Starting a session of an application
    Config(..)
  , Handle(..)
  , newHandle
  ) where


import           Blaze.Core
import qualified Blaze.Development.Internal.Logger as Logger
import           Blaze.Development.Internal.Types
                 ( RenderableApp(..), HtmlApp,
                 )
import qualified Blaze.Development.ProxyApi                as ProxyApi
import qualified Blaze.Development.Server.ResourceManager  as ResourceManager

import           Control.Concurrent           (threadDelay)
import qualified Control.Concurrent.Async     as Async
import           Control.Concurrent.STM.TVar
                 ( TVar, newTVarIO, readTVar, writeTVar, readTVarIO
                 )
import           Control.Exception            (finally, evaluate, catch, bracket_)
import           Control.Lens                 (preview, _Left)
import           Control.Monad
import           Control.Monad.STM            (STM, atomically)
import           Control.Monad.State

import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Lazy         as BL
import           Data.Maybe
import           Data.Monoid

import           Numeric                         (showHex)

import           System.Directory
                 ( renameFile, removeFile, doesFileExist, doesDirectoryExist
                 , createDirectoryIfMissing
                 )
import           System.FilePath              (takeDirectory)
import           System.IO
                 ( withBinaryFile, IOMode(ReadMode, WriteMode)
                 )
import           System.Random                (getStdRandom, randomR)

import qualified Text.Blaze.Event.Internal    as EI


------------------------------------------------------------------------------
-- Configuration and Handle
------------------------------------------------------------------------------

data Config = Config
    { cStateFile :: !(Maybe FilePath)
      -- ^ Path to the file that should be used to persist the application
      -- state, if at all.
    } deriving (Eq, Show)

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


------------------------------------------------------------------------------
-- Session start
------------------------------------------------------------------------------

-- | Create a new handle that allows running with a single instance of an
-- HtmlApp.
newHandle
    :: forall st act.
       (Aeson.ToJSON st, Aeson.FromJSON st)
    => Config
    -> Logger.Handle
    -> ResourceManager.Handle
       -- ^ Resource manager to which we should bind our resources. We'll use
       -- this manager to control the state writer thread that we start, when
       -- persisting the state on changes.
    -> HtmlApp st act
    -> IO Handle
newHandle
    (Config mbStateFile) loggerH resourceManagerH (RenderableApp render app)
  = do
    -- allocate state reference
    stVar <- case mbStateFile of
        Nothing        -> newTVarIO (appInitialState app, 0)
        Just stateFile -> do
          -- ensure that the containing directory exists
          ensureContainingDirectoryExists loggerH stateFile
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
    currentRevId <- snd <$> readTVarIO stVar
    case mbStateFile of
      Nothing        -> return ()
      Just stateFile -> void $ ResourceManager.async resourceManagerH $
          bracket_
              (Logger.logInfo loggerH $ "Starting state-logger on '" <> stateFile <> "'.")
              (Logger.logInfo loggerH $ "Stopping state-logger on '" <> stateFile <> "'.")
              (persistStateOnChange stateFile stVar currentRevId)

    -- execute the initial request
    runIORequest (appInitialRequest app) (applyActionIO stVar)

    -- return event handlers
    return $ Handle (handleGetView stVar) (handleHandleEvent stVar)
  where
    persistStateOnChange
        :: FilePath -> TVar (st, ProxyApi.RevisionId) -> ProxyApi.RevisionId -> IO ()
    persistStateOnChange stateFile stVar currentRevId = do
        st <- atomically $ do
            st@(_, revId) <- readTVar stVar
            guard (currentRevId /= revId)
            return st
        -- write file and persist state on a change
        writeFileAsJson stateFile st `catch` handleIOError
        persistStateOnChange stateFile stVar (snd st)
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
              then logInfo' $
                     "event revision-id " <> show evRevId <>
                     " does not match state revision-id " <> show revId <> "."
              else
                case lookupByPosition pos (render st) of
                  Nothing -> logInfo' $
                    "ignore event that we cannot locate at position " <>
                    show pos <> "."
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
-- File storage
------------------------------------------------------------------------------

ensureContainingDirectoryExists :: Logger.Handle -> FilePath -> IO ()
ensureContainingDirectoryExists loggerH file = do
    doesExist <- doesDirectoryExist dir
    unless doesExist $ do
        Logger.logInfo loggerH $ "Creating state directory: " <> dir
        createDirectoryIfMissing True dir
  where
    dir = takeDirectory file

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

