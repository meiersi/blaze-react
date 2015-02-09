{-# LANGUAGE OverloadedStrings #-}

module Blaze2.ReactJS.Examples.SocketTest
    ( renderState
    , handleRequest
    ) where

import           Blaze2.Core.Examples.SocketTest
import           Blaze2.Core.Service.Socket
import           Blaze2.ReactJS.Base

import           Control.Monad (forM_)
import           Data.Monoid ((<>))

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Event            as E
import qualified Text.Blaze.Event.Keycode    as Keycode

renderState :: SocketTestS -> WindowState SocketTestA
renderState (SocketTestS socketOpen messages inputBox) = WindowState
    { _wsPath = ""
    , _wsBody = do
        H.p $ "Socket " <> if socketOpen then "open" else "closed"
        H.ol $ forM_ (reverse messages) $ H.li . H.toHtml . show
        H.input H.! A.type_ "text"
                H.! A.placeholder "Message"
                H.! A.value (H.toValue inputBox)
                H.! E.onValueChange UpdateInputA
                H.! E.onKeyDown [Keycode.enter] SubmitInputA
    }

handleRequest :: Socket -> (SocketTestA -> IO ()) -> SocketTestR -> IO ()
handleRequest sock chan = mapM_ $ sock (chan . SA)
