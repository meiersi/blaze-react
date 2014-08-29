
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Blaze.React.Run.ReactJS (runApp) where


import           Blaze.React               (App(..))

import           Control.Applicative
import           Control.Concurrent        (threadDelay, forkIO)
import           Control.Exception         (bracket)
import           Control.Monad

import           Data.IORef
import           Data.Maybe            (fromMaybe)

import           GHCJS.Types           (JSRef, JSString, JSObject, JSFun)
import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Foreign.QQ      (js, js_)

import           Prelude hiding (div)

import           System.IO             (fixIO)

import qualified Text.Blaze.Renderer.ReactJS    as ReactJS



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
    "window.requestAnimationFrame($1)"
    requestAnimationFrame :: JSFun (IO ()) -> IO ()

atAnimationFrame :: IO () -> IO ()
atAnimationFrame io = do
    cb <- fixIO $ \cb ->
        Foreign.syncCallback Foreign.AlwaysRetain
                             False
                             (Foreign.release cb >> io)
    requestAnimationFrame cb

runApp :: (Show act) => App st act -> IO ()
runApp (App initialState initialRequests apply renderAppState) = do
    -- create root element in body for the app
    root <- [js| document.createElement('div') |]
    [js_| document.body.appendChild(`root); |]

    -- state variables
    stateVar           <- newIORef initialState  -- The state of the app
    redrawScheduledVar <- newIORef False         -- True if a redraw was scheduled
    rerenderVar        <- newIORef Nothing       -- IO function to actually render the DOM

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

    -- create render callback for initialState
    let handleAction action requireSyncRedraw = do
            putStrLn $ "runApp - applying action: " ++ show action ++
                       (if requireSyncRedraw then " (sync redraw)" else "")
            requests <- atomicModifyIORef' stateVar (\state -> apply action state)
            handleRequests requests
            if requireSyncRedraw then syncRedraw else asyncRedraw
        handleRequests requests = do
          forM_ requests $ \req -> forkIO $ do
            action <- req
            handleAction action False


        mkRenderCb :: IO (JSFun (JSObject ReactJS.ReactJSNode -> IO ()))
        mkRenderCb = do
            Foreign.syncCallback1 Foreign.AlwaysRetain False $ \objRef -> do
                state <- readIORef stateVar
                node <- ReactJS.renderHtml handleAction (renderAppState state)
                Foreign.setProp ("node" :: JSString) node objRef



    -- mount and redraw app
    bracket mkRenderCb Foreign.release $ \renderCb -> do
        app <- mountReactApp root renderCb
        -- manually tie the knot between the event handlers
        writeIORef rerenderVar (Just (syncRedrawApp app))
        -- start the first drawing
        syncRedraw
        -- handle the initial requests
        handleRequests initialRequests
        -- keep main thread running forever
        forever $ threadDelay 10000000

