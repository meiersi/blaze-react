{-# LANGUAGE OverloadedStrings #-}

module Blaze.React.Backend.ReactJS.Run
    ( runApp
    , runApp'
    ) where


import           Blaze.React.Backend.ReactJS.Base
import           Blaze.React.Core
import qualified Blaze.React.Html5                     as H
import qualified Blaze.React.Html5.Event               as E
import qualified Blaze.React.Html5.Renderer.ReactJS    as ReactJS

import           Control.Applicative
import           Control.Concurrent        (threadDelay)
import           Control.Exception         (bracket)
import           Control.Monad

import           Data.IORef
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import           Data.Time.Clock       (getCurrentTime)

import           GHCJS.Types           (JSVal)
import qualified GHCJS.Marshal.Pure    as Marshal

import           GHCJS.Foreign.Callback (Callback)
import qualified GHCJS.Foreign.Callback as Foreign

import           JavaScript.Object.Internal (Object(..))
import qualified JavaScript.Object          as Object

import           Data.JSString         (JSString)
import qualified Data.JSString.Text    as JSString

import           Prelude hiding (div)

import           System.IO             (fixIO)


------------------------------------------------------------------------------
-- Generic 'runApp' function based on reactjs
------------------------------------------------------------------------------


-- ISSUES:
--   * 'this' in callbacks
--   * how to return a value from a sync callback


-- | A type-tag for an actual Browser DOM node.
type DOMNode = JSVal
type ReactJSApp = JSVal

foreign import javascript unsafe
    "h$reactjs.mountApp($1, $2)"
    mountReactApp
        :: DOMNode                          -- ^ Browser DOM node
        -> Callback (JSVal -> IO ())
           -- ^ render callback that stores the created nodes in the 'node'
           -- property of the given object.
        -> IO ReactJSApp

foreign import javascript unsafe
    "h$reactjs.syncRedrawApp($1)"
    syncRedrawApp :: ReactJSApp -> IO ()

foreign import javascript unsafe
    "h$reactjs.attachRouteWatcher($1)"
    attachPathWatcher
        :: Callback (JSVal -> IO ())
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
    requestAnimationFrame :: Callback (IO ()) -> IO ()

foreign import javascript unsafe
    "document.createElement(\"div\")"
    documentCreateDiv :: IO DOMNode

foreign import javascript unsafe
    "document.body.appendChild($1)"
    documentBodyAppendChild :: DOMNode -> IO ()

foreign import javascript unsafe
    "location.hash"
    getLocationFragment :: IO JSString


atAnimationFrame :: IO () -> IO ()
atAnimationFrame io = do
    cb <- fixIO $ \cb ->
        Foreign.syncCallback Foreign.ThrowWouldBlock
                             (Foreign.releaseCallback cb >> io)
    requestAnimationFrame cb

-- | Run an application that is indifferent to the hash-fragment of the path.
runApp'
    :: (st -> H.Html (E.EventHandler act))
    -> App st act ((act -> IO ()) -> IO ())
    -> IO ()
runApp' render0 app0 =
    runApp render app
  where
    render st = WindowState (E.mapActions Right (render0 st)) ""
    app = App
      { appInitialState   = appInitialState app0
      , appInitialRequest = appInitialRequest app0
      , appApplyAction    = \act0 st -> case act0 of
          Left _    -> (st, const (return ()))
          Right act -> appApplyAction app0 act st
      }

runApp
    :: (st -> WindowState (Either WindowAction act))
    -> App st (Either WindowAction act) ((act -> IO ()) -> IO ())
    -> IO ()
runApp renderState (App initialState initialRequest apply) = do
    -- create root element in body for the app
    root <- documentCreateDiv
    documentBodyAppendChild root

    -- state variables
    stateVar           <- newIORef initialState  -- The state of the app
    redrawScheduledVar <- newIORef False         -- True if a redraw was scheduled
    rerenderVar        <- newIORef Nothing       -- IO function to actually render the DOM

    -- This is a cache of the URL fragment (hash) to prevent unnecessary
    -- updates.
    urlFragmentVar <- newIORef =<< JSString.textFromJSString <$> getLocationFragment

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
            setRoute $ JSString.textToJSString $ "#" <> newPath

    -- create render callback for initialState
    let handleAction requireSyncRedraw action = do
            now <- getCurrentTime
            putStrLn $
                show now ++ " - runApp - applied action" ++
                (if requireSyncRedraw then " (with sync redraw)" else "")
            request <- atomicModifyIORef' stateVar (\state -> apply action state)
            handleRequest request
            if requireSyncRedraw then syncRedraw else asyncRedraw
        handleRequest request =
            void $ request (handleAction False . Right)


        mkRenderCb :: IO (Callback (JSVal -> IO ()))
        mkRenderCb = do
            Foreign.syncCallback1 Foreign.ThrowWouldBlock $ \objRef -> do
                state <- readIORef stateVar
                let (WindowState body path) = renderState state
                updatePath path
                node <- ReactJS.renderHtml (flip handleAction) body
                Object.setProp ("node" :: JSString) node (Object objRef)

    onPathChange <- Foreign.syncCallback1 Foreign.ThrowWouldBlock $
      \pathStr -> do
        currentPath <- readIORef urlFragmentVar
        let newPath = T.drop 1 $ Marshal.pFromJSVal pathStr
        -- FIXME (asayers): if the route is the same, it seems to trigger a
        -- full-page reload
        unless (newPath == currentPath) $ do
          writeIORef urlFragmentVar newPath
          handleAction True (Left $ PathChangedTo newPath)
    attachPathWatcher onPathChange

    -- mount and redraw app
    bracket mkRenderCb Foreign.releaseCallback $ \renderCb -> do
        app <- mountReactApp root renderCb
        -- manually tie the knot between the event handlers
        writeIORef rerenderVar (Just (syncRedrawApp app))
        -- start the first drawing
        syncRedraw
        -- handle the initial requests
        handleRequest initialRequest
        -- keep main thread running forever
        forever $ threadDelay 10000000


