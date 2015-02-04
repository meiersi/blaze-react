module Blaze2.ReactJS.Service.Store
    ( handleRequest
    ) where

import Blaze2.Core.Service.Store

import Control.Applicative
import Data.IORef

-- TODO (asayers): This should use HTML5 local storage
handleRequest :: IORef v -> (StoreA v -> IO ()) -> StoreR v -> IO ()
handleRequest ref channel req = case req of
    ReadR      -> channel =<< ReadA <$> readIORef ref
    WriteR val -> writeIORef ref val
