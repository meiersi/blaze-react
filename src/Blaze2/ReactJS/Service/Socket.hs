{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An implementation of the Socket service which uses websockets.
module Blaze2.ReactJS.Service.Socket
    ( newSocket
    ) where

import           Blaze2.Core.Service.Socket

import           Data.IORef
import qualified Data.Text      as T

import           GHCJS.Types
import           GHCJS.Foreign


foreign import javascript unsafe
    "new WebSocket($1, $2)"
    createWebSocket_ :: JSString -> JSArray JSString -> IO (JSRef JSWebSocket)

foreign import javascript unsafe
    "$1.onopen = $2"
    onOpen_ :: JSRef JSWebSocket -> JSFun (JSRef JSEvent -> IO ()) -> IO ()

foreign import javascript unsafe
    "$1.onmessage = $2"
    onMessage_ :: JSRef JSWebSocket -> JSFun (JSRef JSEvent -> IO ()) -> IO ()

foreign import javascript unsafe
    "$1.send($2)"
    sendMessage_ :: JSRef JSWebSocket -> JSString -> IO ()

-- | Type tags for javascript objects
data JSWebSocket
data JSEvent

-- | This is the mutable state maintained by an instantiation of this service.
-- Each instantiation can handle at most one websocket.
newtype WSState = WSState (Maybe (JSRef JSWebSocket))

setCallback
    :: (JSFun (JSRef JSEvent -> IO ()) -> IO ())
    -> (JSRef JSEvent -> IO ())
    -> IO ()
setCallback setCallback_ callback = do
    callback_ <- syncCallback1 AlwaysRetain False callback
    setCallback_ callback_

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
        setCallback (onOpen_ webSocket) $ \_e -> chan SocketOpened
        setCallback (onMessage_ webSocket) $ \e -> do
          mbMessage <- getPropMaybe ("data" :: JSString) e
          case mbMessage of
            Nothing      -> return () -- Hopefully unreachable
            Just message -> chan $ MessageReceived (fromJSString message)

closeWebSocket :: IORef WSState -> (SocketA -> IO ()) -> IO ()
closeWebSocket wsRef chan = undefined

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


