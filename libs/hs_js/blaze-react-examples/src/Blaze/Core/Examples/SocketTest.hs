{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blaze.Core.Examples.SocketTest
    ( app

    , SocketTestS(..)
    , SocketClosedS(..)
    , SocketConnectingS(..)
    , SocketOpenS(..)
    , SocketTestA(..)
    , SocketTestR(..)
    , Message(..), Sender(..)
    ) where

import           Blaze.Core
import           Blaze.Core.Service.Socket

import           Control.Lens (makeLenses, (.=), (%=), use, makePrisms)

import           Data.Monoid   (Monoid)
import qualified Data.Text     as T
import           Data.Typeable (Typeable)


data SocketTestS
    = STClosed SocketClosedS
    | STConnecting SocketConnectingS
    | STOpen SocketOpenS
    deriving (Show, Typeable)

data SocketClosedS = SocketClosedS
    { _scInputBox :: T.Text
    } deriving Show

data SocketConnectingS = SocketConnectingS
    { _scgTarget :: Url
    } deriving Show

data SocketOpenS = SocketOpenS
    { _soTarget     :: Url
    , _soMessages   :: [Message]   -- Stored in reverse order
    , _soInputBox   :: T.Text
    } deriving Show

data Message = Message !Sender !T.Text deriving Show
data Sender = Us | Them deriving Show


data SocketTestA
    = UpdateInputA T.Text
    | SubmitInputA
    | CloseConnection
    | SA SocketA
    deriving (Show, Typeable)

newtype SocketTestR = SocketTestR [SocketR] deriving (Monoid, Typeable, Show)

makeLenses ''SocketClosedS
makeLenses ''SocketConnectingS
makeLenses ''SocketOpenS
makePrisms ''SocketTestS


applyAction :: SocketTestA -> SocketTestS -> (SocketTestS, SocketTestR)
applyAction act st = case st of
    STClosed     _ -> runApplyActionM (applyActionClosed     act) st
    STConnecting _ -> runApplyActionM (applyActionConnecting act) st
    STOpen       _ -> runApplyActionM (applyActionOpen       act) st

-- Update the input field.
-- If the user hits enter, submit an OpenSocket request and move to the
-- STConnecting state.
applyActionClosed
    :: SocketTestA
    -> ApplyActionM SocketTestS SocketTestR ()
applyActionClosed act = case act of
    UpdateInputA txt ->
      _STClosed . scInputBox .= txt
    SubmitInputA -> do
      url <- use (_STClosed . scInputBox)
      submitRequest $ SocketTestR [OpenSocket url []]
      writeState $ STConnecting $ SocketConnectingS url
    _ -> return ()

-- If the socket opens correctly, move to the STOpen state.
-- If there's an error, move back to the STClosed state.
applyActionConnecting
    :: SocketTestA
    -> ApplyActionM SocketTestS SocketTestR ()
applyActionConnecting act = case act of
    SA (SocketOpened _) -> do
      url <- use (_STConnecting . scgTarget)
      writeState $ STOpen $ SocketOpenS url [] ""
    SA (SocketError _) -> do
      url <- use (_STConnecting . scgTarget)
      writeState $ STClosed $ SocketClosedS url
    _ -> return ()

applyActionOpen
    :: SocketTestA
    -> ApplyActionM SocketTestS SocketTestR ()
applyActionOpen act = case act of
    UpdateInputA txt ->
      _STOpen . soInputBox .= txt
    SubmitInputA -> do
      message <- use (_STOpen . soInputBox)
      submitRequest $ SocketTestR [SendMessage message]
      _STOpen . soMessages %= (Message Us message :)
      _STOpen . soInputBox .= ""
    CloseConnection ->
      submitRequest $ SocketTestR [CloseSocket]
    SA SocketClosed ->
      writeState initialState
    SA (MessageReceived message) ->
      _STOpen . soMessages %= (Message Them message :)
    SA (SocketError _) ->
      return ()
    SA (SocketOpened _) ->
      return ()


initialState :: SocketTestS
initialState = STClosed $ SocketClosedS "ws://echo.websocket.org"

app :: App SocketTestS SocketTestA SocketTestR
app = App
    { appInitialState = initialState
    , appInitialRequest = SocketTestR []
    , appApplyAction = applyAction
    }
