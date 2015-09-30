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

import           Servant.API
import           Servant.Client


main :: IO ()
main = do
    -- create logger - obvious comment is obvious (TC)
    loggerH <- Logger.newStdoutLogger
    -- TODO (SM): extract server-url from environment

    -- define client application
    let serverH = Client.Handle
          { Client.hLogger    = loggerH
          , Client.hPostEvent = showErrors . postEvent
          , Client.hGetView   = showErrors . getView
          }
        clientApp = Client.clientAppFor serverH (T.pack (show serverUrl))

    -- run client application using React.js
    runApp' (raRender clientApp) (runIORequest <$> raApp clientApp)
  where
    postEvent :<|> getView = client ProxyApi.api serverUrl

    serverUrl  = BaseUrl Http "localhost" 8081
    -- TODO (SM): show the errors once they do not use 'undefined' in the
    -- 'ghcjs-servant-client' package anymore.
    showErrors = bimapEitherT (const "Some ServantError.") id
    -- showErrors = bimapEitherT show id



