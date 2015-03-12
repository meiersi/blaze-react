{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An implementation of the Socket service which uses websockets.
module Blaze.ReactJS.Service.Socket
    ( newSocket
    ) where

import           Blaze.Core.Service.Socket

import           Data.IORef
import qualified Data.Text      as T

import           GHCJS.Types
import           GHCJS.Foreign


foreign import javascript unsafe
    "new WebSocket($1, $2)"
    createWebSocket_ :: JSString -> JSArray JSString -> IO (JSRef JSWebSocket)

foreign import javascript unsafe
    "$1.send($2)"
    sendMessage_ :: JSRef JSWebSocket -> JSString -> IO ()

foreign import javascript unsafe
    "$1.close()"
    closeWebSocket_ :: JSRef JSWebSocket -> IO ()

-- | Type tags for javascript objects
data JSWebSocket
data JSEvent

-- | This is the mutable state maintained by an instantiation of this service.
-- Each instantiation can handle at most one websocket.
newtype WSState = WSState (Maybe (JSRef JSWebSocket))

-- | Just a helper function for setting callbacks on the websocket object
setCallback
    :: JSRef JSWebSocket
    -> JSString
    -> (JSRef JSEvent -> IO ())
    -> IO ()
setCallback webSocket property callback = do
    callback_ <- syncCallback1 AlwaysRetain False callback
    setProp property callback_ webSocket

-- | FIXME (asayers): Leaks memory. Since this will probably only be called
-- once in the lifetime of an application, it's not so bad. It should still be
-- addressed though.
openWebSocket
    :: IORef WSState
    -> (SocketA -> IO ())
    -> T.Text
    -> [T.Text]
    -> IO ()
openWebSocket wsRef chan url protocols = do
    (WSState mbWebSocket) <- readIORef wsRef
    case mbWebSocket of
      Just _  ->
        chan $ SocketError "socket already open"
      Nothing -> do
        protocols_ <- toArray $ map (castRef . toJSString) protocols
        webSocket <- createWebSocket_ (toJSString url) protocols_
        writeIORef wsRef (WSState $ Just webSocket)
        setCallback webSocket "onopen" $ \_e -> do
          mbProtocol <- getPropMaybe ("protocol" :: JSString) webSocket
          case mbProtocol of
            Nothing       -> chan $ SocketError "couldn't read protocol"
            Just protocol -> chan $ SocketOpened (fromJSString protocol)
        setCallback webSocket "onclose" $ \_e -> do
          writeIORef wsRef (WSState Nothing)
          -- TODO (asayers): Return the reason the socket was closed
          chan SocketClosed
        setCallback webSocket "onerror" $ \_e ->
          -- FIXME (asayers): This isn't very descriptive
          chan $ SocketError "websocket error"
        setCallback webSocket "onmessage" $ \e -> do
          mbMessage <- getPropMaybe ("data" :: JSString) e
          case mbMessage of
            Nothing      -> chan $ SocketError "couldn't read message"
            Just message -> chan $ MessageReceived (fromJSString message)

closeWebSocket :: IORef WSState -> (SocketA -> IO ()) -> IO ()
closeWebSocket wsRef chan = do
    (WSState mbWebSocket) <- readIORef wsRef
    case mbWebSocket of
      Nothing ->
        chan $ SocketError "socket already closed"
      Just webSocket ->
        closeWebSocket_ webSocket

sendMessage
    :: IORef WSState
    -> (SocketA -> IO ())
    -> T.Text
    -> IO ()
sendMessage wsRef chan message = do
    (WSState mbWebSocket) <- readIORef wsRef
    case mbWebSocket of
      Nothing ->
        chan $ SocketError "socket not open"
      Just webSocket -> do
        sendMessage_ webSocket (toJSString message)


newSocket :: IO Socket
newSocket = do
    ws <- newIORef $ WSState Nothing
    return $ handleRequest ws

handleRequest
    :: IORef WSState
    -> (SocketA -> IO ())
    -> SocketR
    -> IO ()
handleRequest ws chan req =
    case req of
      OpenSocket url protocols -> openWebSocket ws chan url protocols
      CloseSocket -> closeWebSocket ws chan
      SendMessage message -> sendMessage ws chan message


