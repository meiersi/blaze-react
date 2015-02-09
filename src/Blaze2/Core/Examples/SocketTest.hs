{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Blaze2.Core.Examples.SocketTest
    ( app

    , SocketTestS(..)
    , SocketTestA(..)
    , SocketTestR
    ) where

import           Blaze2.Core
import           Blaze2.Core.Service.Socket

import           Control.Lens (makeLenses, (.=), (%=), use)

import qualified Data.Text as T


data SocketTestS = SocketTestS
    { _stSocketOpen :: Bool
    , _stMessages   :: [(Sender, T.Text)]   -- Stored in reverse order
    , _stInputBox   :: T.Text
    } deriving (Show)

data Sender = Us | Them deriving Show

data SocketTestA
    = UpdateInputA T.Text
    | SubmitInputA
    | SA SocketA
    deriving Show

type SocketTestR = [SocketR]

makeLenses ''SocketTestS

applyAction :: SocketTestA -> ApplyActionM SocketTestS SocketTestR ()
applyAction act = case act of
    UpdateInputA txt -> stInputBox .= txt
    SubmitInputA -> do
      message <- use stInputBox
      submitRequest [SendMessage message]
      stMessages %= ((Us, message):)
      stInputBox .= ""
    SA SocketOpened -> stSocketOpen .= True
    SA SocketClosed -> stSocketOpen .= False
    SA (MessageReceived message) -> stMessages %= ((Them, message):)
    SA (SocketError _) -> return ()

app :: App SocketTestS SocketTestA SocketTestR
app = App
    { appInitialState = SocketTestS False [] ""
    , appInitialRequest = [OpenSocket "ws://echo.websocket.org" []]
    , appApplyAction = runApplyActionM . applyAction
    }
