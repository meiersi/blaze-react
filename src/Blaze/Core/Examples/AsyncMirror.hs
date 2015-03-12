{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | An app that allows to play the client for an arbitrary other app running
-- on the server.
module Blaze.Core.Examples.AsyncMirror
  ( mirror
  , serveMirror
  , ReactJSEvent(..)
  ) where

-- import Control.Applicative
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Lens                (preview, _Left)
import Control.Monad
import Control.Monad.STM           (STM, atomically)
import Control.Monad.State

import Data.Traversable

import Blaze.Core


import Prelude hiding (lookup)


data ReactJSEvent = ReactJSEvent -- placeholder

type BlazeHtml a = [a]

-- | Client state encapsulating server-state 'ss' and view 'v'.
type MirrorS st = Maybe (RevId, st, BlazeHtml Int)

-- | Report that the server failed to return a new state or that there is a
-- new state.
data MirrorA st
    = UpdateReflectionA !RevId !st !(BlazeHtml Int)
      -- ^ Update the state and view of the reflection that we are
      -- maintaining.
    | HandleEventA !RevId !Int !ReactJSEvent
      -- ^ Handle the event that was triggered in our view, which is the
      -- rendered version of the state with the given revision-id.

data MirrorR
    = GetReflectionR !(Maybe RevId)
      -- ^ Request the next mirror-image, which must have a revision-id that
      -- is larger than the one that we are currently displaying.
    | HandleEventR !RevId !Int !ReactJSEvent

-- | Create an app for proxying the session with the given 'SessionId', which
-- is usually chosen randomly.
--
-- We assume that each request for a next server state will be answered with
-- either 'Nothing' or a new server-state.
--
-- TODO (SM): this is more like mirroring a session over an unreliable channel
-- => consider a rename.
mirror :: App (MirrorS st) (MirrorA st) [MirrorR]
mirror = App
    { appInitialState   = Nothing
    , appInitialRequest = [GetReflectionR Nothing]
    , appApplyAction    = \act -> runApplyActionM $ do
        case act of
          HandleEventA revId pos ev ->
            submitRequest [HandleEventR revId pos ev]
          UpdateReflectionA revId st view -> do
            writeState (Just (revId, st, view))
            submitRequest [GetReflectionR (Just revId)]
    }
{-
  do
    -- setup capturing server
    (handleEvent, handleGetReflection) <- startServer app
    let evalMirrorR GetNextReflectionR = handleGetReflection
        evalMirrorR HandleEventR = handleGetReflection

        renderMirror :: BlazeHtml Int -> BlazeHtml (ReactJSEvent -> IO act)

    -- run blaze react on mirrored app
    runBlazeReact (evalMirrorR <$> mirror)
-}

lookupById :: Traversable f => Int -> f a -> Maybe a
lookupById i t =
    preview _Left $ execStateT (traverse lookup t) 0
  where
    lookup x = do
        nextId <- get
        put (succ nextId)
        when (nextId == i) (lift (Left x))


annotateWithId :: Traversable f => f a -> f Int
annotateWithId t =
    evalState (traverse annotate t) 0
  where
    annotate _x = do
        nextId <- get
        put (succ nextId)
        return nextId


type IORequest act = (act -> IO ()) -> IO ()

type RevId = Int

serveMirror
    :: forall st act.
       (st -> BlazeHtml (ReactJSEvent -> Maybe act))
    -> App st act (IORequest act)
    -> IO ( Maybe RevId -> IO (BlazeHtml Int, RevId)
          , Int -> ReactJSEvent -> RevId -> IO ()
          )
serveMirror render app = do
    -- allocate state reference
    stVar <- newTVarIO (appInitialState app, 0)

    -- execute the initial request
    appInitialRequest app (applyActionIO stVar)

    -- return event handlers
    return (handleGetReflection stVar, handleHandleEvent stVar)
  where
    applyAction :: TVar (st, RevId) -> act -> STM (IORequest act)
    applyAction stVar act = do
        (st, revId) <- readTVar stVar
        let (!st', !req) = appApplyAction app act st
        writeTVar stVar (st', succ revId)
        return req

    applyActionIO :: TVar (st, RevId) -> act -> IO ()
    applyActionIO stVar act = do
        req <- atomically (applyAction stVar act)
        req (applyActionIO stVar)

    renderWithId = annotateWithId . render

    -- function to handle a 'HandleEvent' mirror-request
    handleHandleEvent stVar pos ev evRevId = do
        req <- atomically $ do
            (st, revId) <- readTVar stVar
            case lookupById pos (render st) of
              Nothing -> return emptyRequest-- ignore event that we cannot locate
              Just evToAct
                | revId /= evRevId -> return emptyRequest -- event does not match revision-id
                | otherwise        -> do
                    case evToAct ev of
                      Nothing  -> return emptyRequest -- event does not result in an action
                      Just act -> applyAction stVar act
        -- execute resulting request
        req (applyActionIO stVar)
      where
        emptyRequest _applyActionIO = return ()

    -- function to handle a 'GetReflection' mirror-request
    handleGetReflection stVar mbClientRevId = atomically $ do
        (st, revId) <- readTVar stVar
        -- only return an update once the revision-id has changed
        guard (mbClientRevId /= Just revId)
        return (renderWithId st, revId)








{-

type CaptureS st

type CaptureA st act = MirrorR st

type CaptureR st req = ([MirrorA st], [req])


capture
    :: App st act req
    -> (st -> BlazeHtml (ReactJSEvent -> action))
       -- ^ Render function
    -> App (CaptureS st) (CaptureA act) (CaptureR req)
capture app render = App
    { appInitialState   = Nothing
    , appInitialRequest = [GetNextReflectionR Nothing]
    , appApplyAction    = \act -> runApplyActionM $ do
        case act of
          Left (HandleEventR pos ev ss) ->
            case lookupById pos (render ss) of
              Nothing      -> return () -- bogus event => ignore
              Just evToAct ->
                let act = evToAct ev
                    -- transition inner app state
                    (ss', req) = appApplyAction app act ss
                submitRequest [L req]

            submitRequest (UpdateReflectionA (ss', renderWithId ss'), mempty)

          Left (GetNextReflectionR Nothing) ->
            let ss = appInitialState app
            submitRequest (UpdateReflectionA (ss, renderWithId ss), mempty)

          Left (GetNextReflectionR (Just ss)) ->
            submitRequest [Left

          Right act ->

    }
  where
    renderWithId = annotateWithId . render



-}
