

-- ISSUES:
--
-- - The interface should support compare-and-swap, because the LocalStorage
--   could easily have changed underneath you (eg. if multiple instances of the
--   app are running).
-- - The API is pull-based, meaning that if users want to watch for changes to
--   the storage, they have to do polling. We could implement some API where an
--   app subscribes to an action stream which shows writes.

module Blaze2.ReactJS.Service.Store
    ( handleRequest
    ) where

import Blaze2.Core.Service.Store

import Data.Maybe

import GHCJS.Foreign
import GHCJS.Types

import Safe (readMay)


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

readLocalStorage :: String -> IO (Maybe String)
readLocalStorage key = do
    valRef <- readLocalStorage_ (toJSString key)
    if isNull valRef
      then return Nothing
      else return $ Just $ fromJSString valRef

writeLocalStorage :: String -> String -> IO ()
writeLocalStorage key val =
    writeLocalStorage_ (toJSString key) (toJSString val)


-- The request handler
-------------------------------------------------------------------------------

-- TODO (asayers): Better to use ToJSON/FromJSON contraints, and then to
-- serialise the JSON to strings. Note that we can't store the values as
-- javascript objects, because HTML5 LocalStorage only accepts strings :(
handleRequest
    :: (Read v, Show v)
    => String -> v -> (StoreA v -> IO ()) -> StoreR v -> IO ()
handleRequest storeName defaultVal channel req =
    case req of
      ReadR -> do
          mbValStr <- readLocalStorage storeName
          let mbVal = readMay =<< mbValStr
          channel $ ReadA $ fromMaybe defaultVal mbVal
      WriteR val ->
          writeLocalStorage storeName $ show val
