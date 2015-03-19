{-# LANGUAGE OverloadedStrings #-}

module Blaze.ReactJS.Examples.SocketTest
    ( renderState
    , handleRequest
    ) where

import           Blaze.Core.Examples.SocketTest
import           Blaze.Core.Service.Socket
import           Blaze.ReactJS.Base

import           Control.Monad (forM_)
import qualified Data.HashMap.Strict as HMS
import           Data.Monoid ((<>))

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Event            as E
import qualified Text.Blaze.Event.Keycode    as Keycode

renderState :: SocketTestS -> WindowState SocketTestA
renderState st = WindowState
    { _wsPath = ""
    , _wsBody = case st of
        STClosed st' -> renderSocketClosed st'
        STConnecting st' -> renderSocketConnecting st'
        STOpen st' -> renderSocketOpen st'
    }

renderSocketClosed :: SocketClosedS -> H.Html SocketTestA
renderSocketClosed (SocketClosedS inputBox) = do
    H.p $ "Not connected. Enter a URL and hit enter:"
    H.input H.! A.type_ "text"
            H.! A.placeholder "Websockets URL"
            H.! A.autofocus True
            H.! A.value (H.toValue inputBox)
            H.! E.onValueChange UpdateInputA
            H.! E.onKeyDown [Keycode.enter] SubmitInputA

renderSocketConnecting :: SocketConnectingS -> H.Html SocketTestA
renderSocketConnecting (SocketConnectingS target) = do
    H.p $ "Connecting to " <> H.toHtml target <> "..."

renderSocketOpen :: SocketOpenS -> H.Html SocketTestA
renderSocketOpen (SocketOpenS target messages inputBox) = do
    H.p $ do
        "Connected to " <> H.toHtml target <> "."
        H.a H.! A.style redLinkStyle
            H.! E.onClick' CloseConnection $ "Close connection"
    H.table $ forM_ (reverse messages) $ \(Message sender msg) ->
      H.tr $ do
        H.td H.! A.style senderStyle $
          H.toHtml $ show sender <> ":"
        H.td H.! A.style (case sender of Us -> usStyle; Them -> themStyle) $
          H.toHtml msg
    H.input H.! A.type_ "text"
            H.! A.placeholder "Message"
            H.! A.autofocus True
            H.! A.value (H.toValue inputBox)
            H.! E.onValueChange UpdateInputA
            H.! E.onKeyDown [Keycode.enter] SubmitInputA
  where
    redLinkStyle = HMS.fromList
      [ ("color"           , "red"       )
      , ("text-decoration" , "underline" )
      , ("margin-left"     , "1em"       )
      ]
    msgStyle  = HMS.fromList [("border", "1px solid black"), ("width", "75%")]
    usStyle   = HMS.union msgStyle $ HMS.fromList [("background-color", "#E4F1FE")]
    themStyle = HMS.union msgStyle $ HMS.fromList [("background-color", "#FDE3A7")]
    senderStyle = HMS.fromList [("text-align", "right")]

handleRequest :: Socket -> SocketTestR -> (SocketTestA -> IO ()) -> IO ()
handleRequest sock (SocketTestR reqs) chan = mapM_ (sock (chan . SA)) reqs
