{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Blaze.ReactJS.Run
    ( runApp
    , runApp'
    ) where


import           Blaze.Core
import           Blaze.ReactJS.Base

import           Control.Applicative
import           Control.Concurrent        (threadDelay, forkIO)
import           Control.Exception         (bracket)
import           Control.Lens              (over)
import           Control.Monad

import           Data.IORef
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T

import           GHCJS.Types           (JSRef, JSString, JSObject, JSFun)
import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Foreign.QQ      (js, js_)

import           Prelude hiding (div)

import qualified Text.Blaze.Renderer.ReactJS    as ReactJS
import qualified Text.Blaze.Event               as E

import           System.IO             (fixIO)


------------------------------------------------------------------------------
-- Generic 'runApp' function based on reactjs
------------------------------------------------------------------------------


-- ISSUES:
--   * 'this' in callbacks
--   * how to return a value from a sync callback


-- | A type-tag for an actual Browser DOM node.
data DOMNode_
data ReactJSApp_

foreign import javascript unsafe
    "h$reactjs.mountApp($1, $2)"
    mountReactApp
        :: JSRef DOMNode_                          -- ^ Browser DOM node
        -> JSFun (JSObject ReactJS.ReactJSNode -> IO ())
           -- ^ render callback that stores the created nodes in the 'node'
           -- property of the given object.
        -> IO (JSRef ReactJSApp_)

foreign import javascript unsafe
    "h$reactjs.syncRedrawApp($1)"
    syncRedrawApp :: JSRef ReactJSApp_ -> IO ()

foreign import javascript unsafe
    "h$reactjs.attachRouteWatcher($1)"
    attachPathWatcher
        :: JSFun (JSString -> IO ())
           -- ^ Callback that handles a route change.
        -> IO ()

foreign import javascript unsafe
    "h$reactjs.setRoute($1)"
    setRoute
        :: JSString
           -- ^ The new URL fragment
        -> IO ()

foreign import javascript unsafe
    "window.requestAnimationFrame($1)"
    requestAnimationFrame :: JSFun (IO ()) -> IO ()

atAnimationFrame :: IO () -> IO ()
atAnimationFrame io = do
    cb <- fixIO $ \cb ->
        Foreign.syncCallback Foreign.AlwaysRetain
                             False
                             (Foreign.release cb >> io)
    requestAnimationFrame cb

runApp'
    :: (Show act)
    => (st -> WindowState act)
    -> App st act ((act -> IO ()) -> IO ())
    -> IO ()
runApp' renderState app =
    runApp (over wsBody (E.mapActions Right) . renderState)
           (ignoreActions (const $ return ()) app)

runApp
    :: (Show act)
    => (st -> WindowState (Either WindowAction act))
    -> App st (Either WindowAction act) ((act -> IO ()) -> IO ())
    -> IO ()
runApp renderState (App initialState initialRequest apply) = do
    -- create root element in body for the app
    root <- [js| document.createElement('div') |]
    [js_| document.body.appendChild(`root); |]

    -- state variables
    stateVar           <- newIORef initialState  -- The state of the app
    redrawScheduledVar <- newIORef False         -- True if a redraw was scheduled
    rerenderVar        <- newIORef Nothing       -- IO function to actually render the DOM

    -- This is a cache of the URL fragment (hash) to prevent unnecessary
    -- updates.
    urlFragmentVar <- newIORef =<< Foreign.fromJSString <$> [js|location.hash|]

    -- rerendering
    let syncRedraw = join $ fromMaybe (return ()) <$> readIORef rerenderVar

        asyncRedraw = do
            -- FIXME (meiersi): there might be race conditions
            redrawScheduled <- readIORef redrawScheduledVar
            unless redrawScheduled $ do
                writeIORef redrawScheduledVar True
                atAnimationFrame $ do
                    writeIORef redrawScheduledVar False
                    syncRedraw

    let updatePath newPath = do
          currentPath <- readIORef urlFragmentVar
          unless (newPath == currentPath) $ do
            writeIORef urlFragmentVar newPath
            setRoute $ Foreign.toJSString $ "#" <> newPath

    -- create render callback for initialState
    let handleAction requireSyncRedraw action = do
            putStrLn $ "runApp - applying action: " ++ show action ++
                       (if requireSyncRedraw then " (sync redraw)" else "")
            request <- atomicModifyIORef' stateVar (\state -> apply action state)
            handleRequest request
            if requireSyncRedraw then syncRedraw else asyncRedraw
        handleRequest request =
            void $ forkIO $ request (handleAction False . Right)


        mkRenderCb :: IO (JSFun (JSObject ReactJS.ReactJSNode -> IO ()))
        mkRenderCb = do
            Foreign.syncCallback1 Foreign.AlwaysRetain False $ \objRef -> do
                state <- readIORef stateVar
                let (WindowState body path) = renderState state
                updatePath path
                node <- ReactJS.renderHtml (flip handleAction) body
                Foreign.setProp ("node" :: JSString) node objRef

    onPathChange <- Foreign.syncCallback1 Foreign.AlwaysRetain False $
      \pathStr -> do
        currentPath <- readIORef urlFragmentVar
        let newPath = T.drop 1 $ Foreign.fromJSString pathStr
        -- FIXME (asayers): if the route is the same, it seems to trigger a
        -- full-page reload
        unless (newPath == currentPath) $ do
          writeIORef urlFragmentVar newPath
          handleAction True (Left $ PathChangedTo newPath)
    attachPathWatcher onPathChange

    -- mount and redraw app
    bracket mkRenderCb Foreign.release $ \renderCb -> do
        app <- mountReactApp root renderCb
        -- manually tie the knot between the event handlers
        writeIORef rerenderVar (Just (syncRedrawApp app))
        -- start the first drawing
        syncRedraw
        -- handle the initial requests
        handleRequest initialRequest
        -- keep main thread running forever
        forever $ threadDelay 10000000


