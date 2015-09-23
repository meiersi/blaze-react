-- | Logging support for our development server and client. This module is
-- intended to be imported qualified as 'Logger'.
--
module Blaze.Development.Internal.Logger
  ( Handle(..)
  , newStdoutLogger
  ) where

import           Control.Concurrent.MVar      (newMVar, withMVar)

import           Data.Monoid
import           Data.Time                    ()
import           Data.Time.Clock              (getCurrentTime)


-- | A logger that logs all message on info level.
newtype Handle = Handle
    { logInfo :: String -> IO ()
    }

-- | Allocate a simple logger that outputs messages to 'stdout'.
newStdoutLogger :: IO Handle
newStdoutLogger = do
    mutex <- newMVar ()
    return $ Handle
      { logInfo = \msg -> withMVar mutex $ \_ -> do
            now <- getCurrentTime
            putStrLn $ take 22 (show now) <> " [INFO] " <> msg
      }

