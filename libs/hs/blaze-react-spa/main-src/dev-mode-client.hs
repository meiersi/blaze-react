{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The GHCJS client for the blaze-react dev-mode.
module Main
  ( main
  ) where

import           Blaze.Core
import           Blaze.ReactJS.Run
import qualified Blaze.Development.Client           as Client
import qualified Blaze.Development.Internal.Logger  as Logger
import           Blaze.Development.Internal.Types
                 ( RenderableApp(raRender, raApp)
                 )
import qualified Blaze.Development.ProxyApi         as ProxyApi

import           Control.Monad.Trans.Either   (bimapEitherT)

import qualified Data.Text                    as T
import           Data.Monoid                  ((<>))

import           GHCJS.Types           (JSString)
import qualified GHCJS.Foreign         as Foreign


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
getServerUrl = Foreign.fromJSString <$> js_getServerUrl

foreign import javascript safe
    "$r = BLAZE_REACT_DEV_MODE_SESSION_ID"
    js_getSessionId :: IO JSString

-- | Fetch the server url that was injected via the SERVER-URL.js script.
getSessionId :: IO ProxyApi.SessionId
getSessionId =
    ProxyApi.SessionId . read . Foreign.fromJSString <$> js_getSessionId


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main = do
    loggerH    <- Logger.newStdoutLogger
    serverUrl0 <- getServerUrl
    serverUrl  <- parseBaseUrl serverUrl0
    sid        <- getSessionId

    -- define client application
    let postEvent :<|> getView = client ProxyApi.api serverUrl
        serverH = Client.Handle
          { Client.hLogger    = loggerH
          , Client.hPostEvent = showErrors . postEvent sid
          , Client.hGetView   = showErrors . getView sid
          }
        serverName = T.pack $ serverUrl0 <> "/s/" <> show (ProxyApi.unSessionId sid)
        clientApp = Client.clientAppFor serverH serverName

    -- run client application using React.js
    runApp' (raRender clientApp) (runIORequest <$> raApp clientApp)
  where

    -- TODO (SM): show the errors once they do not use 'undefined' in the
    -- 'ghcjs-servant-client' package anymore.
    showErrors = bimapEitherT (const "Some ServantError.") id
    -- showErrors = bimapEitherT show id

