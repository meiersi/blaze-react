{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The GHCJS client for the blaze-react dev-mode.
module Main
  ( main
  ) where

import           Blaze.React.Backend.ReactJS.Run
import           Blaze.React.Core
import qualified Blaze.React.Development.Client           as Client
import qualified Blaze.React.Development.Internal.Logger  as Logger
import           Blaze.React.Development.Internal.Types
                 ( RenderableApp(raRender, raApp)
                 )
import qualified Blaze.React.Development.ProxyApi         as ProxyApi

import           Control.Monad.Trans.Either   (bimapEitherT)

import qualified Data.Text                    as T

import           Data.JSString         (JSString)
import qualified Data.JSString         as JSString

import           Servant.API
import           Servant.Client


------------------------------------------------------------------------------
-- FFI
------------------------------------------------------------------------------

foreign import javascript safe
    "$r = BLAZE_REACT_DEV_MODE_SERVER_URL"
    js_getServerUrl :: IO JSString

-- | Fetch the server url that was injected via the SERVER-URL.js script.
getServerUrl :: IO String
getServerUrl = JSString.unpack <$> js_getServerUrl


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main = do
    -- create logger - obvious comment is obvious (TC)
    loggerH    <- Logger.newStdoutLogger
    serverUrl0 <- getServerUrl
    serverUrl  <- parseBaseUrl serverUrl0
    -- TODO (SM): extract server-url from environment

    -- define client application
    let postEvent :<|> getView = client ProxyApi.api (Just serverUrl)
        serverH = Client.Handle
          { Client.hLogger    = loggerH
          , Client.hPostEvent = showErrors . postEvent
          , Client.hGetView   = showErrors . getView
          }
        clientApp = Client.clientAppFor serverH (T.pack serverUrl0)

    -- run client application using React.js
    runApp' (raRender clientApp) (runIORequest <$> raApp clientApp)
  where

    -- TODO (SM): show the errors once they do not use 'undefined' in the
    -- 'ghcjs-servant-client' package anymore.
    showErrors = bimapEitherT (const "Some ServantError.") id
    -- showErrors = bimapEitherT show id

