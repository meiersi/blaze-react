{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple client to test the mirroring on the console.
module Blaze.React.Development.Internal.ConsoleRunner
  ( defaultMain
  ) where

import           Blaze.React.Core
import qualified Blaze.React.Development.Internal.Logger as Logger
import           Blaze.React.Development.Internal.Types
                 ( RenderableApp(..), StringApp
                 )

import           Control.Concurrent.STM.TVar  (TVar, newTVarIO, readTVar, writeTVar)
import           Control.Exception            (finally)
import           Control.Monad
import           Control.Monad.STM            (STM, atomically)

import           Data.Monoid


------------------------------------------------------------------------------
-- Application execution
------------------------------------------------------------------------------

newtype Handle act = Handle
    { hApplyAction :: act -> IO ()
    }

newHandle :: forall st act.  Logger.Handle -> StringApp st act -> IO (Handle act)
newHandle loggerH (RenderableApp render app) = do
    stVar <- newTVarIO (appInitialState app)
    -- execute the initial request
    runIORequest (appInitialRequest app) (applyActionIO stVar)
    -- return handle
    return $ Handle (applyActionIO stVar)
  where
    applyAction :: TVar st -> act -> STM (IORequest act, st)
    applyAction stVar act = do
        st <- readTVar stVar
        let (!st', !req) = appApplyAction app act st
        writeTVar stVar st'
        return (req, st')

    applyActionIO :: TVar st -> act -> IO ()
    applyActionIO stVar act = do
        (req, st) <- atomically (applyAction stVar act)
        Logger.logInfo loggerH $
            "Applied action - current view:\n" <> render st
        runIORequest req (applyActionIO stVar)


------------------------------------------------------------------------------
-- Handle application
------------------------------------------------------------------------------

defaultMain :: (Char -> act) -> Logger.Handle -> StringApp st act -> IO ()
defaultMain keypressToAction loggerH app = do
    h <- newHandle loggerH app
    let waitForKeypress = do
          c <- getChar
          hApplyAction h (keypressToAction c)

    Logger.logInfo loggerH
        "Press a key to drive the application; CTRL-C to stop it."
    forever waitForKeypress
        `finally` Logger.logInfo loggerH "Application stopped."


