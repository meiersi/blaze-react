{-# LANGUAGE LambdaCase #-}
-- | A simple console-client for testing a development-mode proxy server.
module Blaze.Development.ConsoleClient
  ( main
  ) where

import qualified Blaze.Development.Client                 as Client
import qualified Blaze.Development.Internal.ConsoleRunner as ConsoleRunner
import qualified Blaze.Development.Internal.Logger        as Logger
import qualified Blaze.Development.ProxyApi               as ProxyApi
import           Blaze.Development.Internal.Types
                 ( StringApp, htmlToStringApp
                 )

import           Control.Monad.Trans.Either   (bimapEitherT)

import           Servant.API
import           Servant.Client


-- | A test-case for running the development client-proxy from the console.
main :: IO ()
main = do
    -- create handle for server
    loggerH <- Logger.newStdoutLogger
    -- run our client proxy using the console-runner
    ConsoleRunner.defaultMain charToAction loggerH $
        clientApp serverUrl loggerH
  where
    serverUrl = BaseUrl Http "localhost" 8081

    testEvent = ProxyApi.Event 0 0 ProxyApi.clickEvSample
    charToAction _c = Client.HandleEventA testEvent


-- | Create a client application given a base-url and logger to use.
clientApp :: BaseUrl -> Logger.Handle -> StringApp Client.State Client.Action
clientApp baseUrl loggerH =
    htmlToStringApp $ Client.clientAppFor handle
  where
    postEvent :<|> getView = client ProxyApi.api baseUrl

    handle = Client.Handle
        { Client.hLogger    = loggerH
        , Client.hPostEvent = adapt . postEvent
        , Client.hGetView   = adapt . getView
        }

    adapt = bimapEitherT show id

