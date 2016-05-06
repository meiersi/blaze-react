{-# LANGUAGE Rank2Types #-}
-- | A resource manager abstraction against which we write the rest of the
-- application.
module Blaze.Development.Server.ResourceManager
  ( ReleaseKey(..)
  , Handle(..)
  , async
  ) where

import qualified Control.Concurrent.Async     as Async

-- | A value that can be used to release an allocated resource.
newtype ReleaseKey = ReleaseKey { release :: IO () }

newtype Handle = Handle
    { allocate :: forall a. IO a -> (a -> IO ()) -> IO (a, ReleaseKey)
    }

-- | Spawn an asynchronous computation that is automatically cancelled when
-- this resource manager is closed.
async :: Handle -> IO a -> IO (Async.Async a, ReleaseKey)
async h io = allocate h (Async.async io) Async.cancel




