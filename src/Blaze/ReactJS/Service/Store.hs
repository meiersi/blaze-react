

-- ISSUES:
--
-- - The interface should support compare-and-swap, because the LocalStorage
--   could easily have changed underneath you (eg. if multiple instances of the
--   app are running).
-- - The API is pull-based, meaning that if users want to watch for changes to
--   the storage, they have to do polling. We could implement some API where an
--   app subscribes to an action stream which shows writes.

module Blaze.ReactJS.Service.Store
    ( handleRequest
    ) where

import           Blaze.Core.Service.Store

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

import           GHCJS.Foreign
import           GHCJS.Types


-- HTML5 Local Storage
-------------------------------------------------------------------------------

-- HTML5 local storage provides a key-value store which is local to both the
-- browser and the domain. The keys and the values must be strings.

foreign import javascript unsafe
    "localStorage.getItem($1)"
    readLocalStorage_ :: JSString -> IO JSString

foreign import javascript unsafe
    "localStorage.setItem($1, $2)"
    writeLocalStorage_ :: JSString -> JSString -> IO ()

readLocalStorage :: T.Text -> IO (Maybe T.Text)
readLocalStorage key = do
    valRef <- readLocalStorage_ (toJSString key)
    if isNull valRef
      then return Nothing
      else return $ Just $ fromJSString valRef

writeLocalStorage :: T.Text -> T.Text -> IO ()
writeLocalStorage key val =
    writeLocalStorage_ (toJSString key) (toJSString val)


-- The request handler
-------------------------------------------------------------------------------

-- NOTE (asayers): We can't store the values as javascript objects, because
-- HTML5 LocalStorage only accepts strings :(
handleRequest
    :: (Aeson.ToJSON v, Aeson.FromJSON v)
    => T.Text -> v -> (StoreA v -> IO ()) -> StoreR v -> IO ()
handleRequest storeName defaultVal channel req =
    case req of
      ReadR -> do
          mbValTxt <- readLocalStorage storeName
          let val = fromMaybe defaultVal $ do
                valTxt <- mbValTxt
                Aeson.decode $ BL.fromStrict $ T.encodeUtf8 valTxt
          channel $ ReadA val
      WriteR val ->
          writeLocalStorage storeName $
            T.decodeUtf8 $ BL.toStrict $ Aeson.encode val
