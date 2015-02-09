
-- | An untyped socket service. Perhaps a better abstractions would be typed
-- sockets, where the types are serialised either as json and sent as text, or
-- as binary and sent as blobs.
module Blaze2.Core.Service.Socket
    ( SocketA(..)
    , SocketR(..)

    , Socket
    , Url
    , Protocol
    ) where

import qualified Data.Text as T

type Socket = (SocketA -> IO ()) -> SocketR -> IO ()

type Url = T.Text
type Protocol = T.Text

data SocketR
    = OpenSocket Url [Protocol]
    | CloseSocket
    | SendMessage T.Text
    deriving (Show, Eq)

data SocketA
    = SocketOpened
    | SocketClosed
    | MessageReceived T.Text
    | MessageSent    -- This isn't very useful without a way to indentify msgs
    | SocketError T.Text
    deriving (Show)
