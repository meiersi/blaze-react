-- | An app that allows to play the client for an arbitrary other app running
-- on the server.
module Blaze2.Core.Examples.AsyncMirror
  ( mirror
  ) where

import Blaze2.Core

data ReactJSEvent = ReactJSEvent -- placeholder

type BlazeHtml a = [a]

-- | Client state encapsulating server-state 'ss' and view 'v'.
type MirrorS ss = Maybe (ss, BlazeHtml Int)

-- | Report that the server failed to return a new state or that there is a
-- new state.
data MirrorA ss
    = UpdateReflectionA !ss !(BlazeHtml Int)
      -- ^ Update the state and view of the reflection that we are
      -- maintaining.
    | HandleEventA !ReactJSEvent
      -- ^ Handle event that was triggered in our view.

data MirrorR
    = GetNextReflectionR
    | HandleEventR !ReactJSEvent

-- | Create an app for proxying the session with the given 'SessionId', which
-- is usually chosen randomly.
--
-- We assume that each request for a next server state will be answered with
-- either 'Nothing' or a new server-state.
--
-- TODO (SM): this is more like mirroring a session over an unreliable channel
-- => consider a rename.
mirror :: App (MirrorS ss) (MirrorA ss) [MirrorR ss]
mirror = App
    { appInitialState   = Nothing
    , appInitialRequest = [GetNextReflectionR]
    , appApplyAction    = \act -> runApplyActionM $ do
        case act of
          HandleEventA ev           -> submitRequest [HandleEventR ev]
          UpdateReflectionA ss view -> do
            writeState (Just (ss, view))
            submitRequest [GetNextReflectionR]
    }

  do 
    -- setup capturing server
    (handleEvent, handleGetReflection) <- startServer app
    let evalMirrorR GetNextReflectionR = handleGetReflection
        evalMirrorR HandleEventR = handleGetReflection

        renderMirror :: BlazeHtml Int -> BlazeHtml (ReactJSEvent -> IO act)

    -- run blaze react on mirrored app
    runBlazeReact (evalMirrorR <$> mirror)

{-
lookupById :: Traversable f => Int -> f a -> Maybe a
lookupById i =
    preview _Left $ execStateT (traverse lookup) 0
  where
    lookup x = do
        nextId <- get
        put (succ nextId)
        when (nextId == i) (lift (Left x))


annotateWithId :: Traversable f => f a -> f (a, Int)
annotateWithId =
    evalState (traverse annotate) 0
  where
    annotate x = do
        nextId <- get
        put (succ nextId)
        return (x, nextId)
-}

type IORequest act = (act -> IO ()) -> IO ()

serveMirror
    :: (st -> BlazeHtml (ReactJSEvent -> Maybe act))
    -> App st act (IORequest act) -> IO ....
serveMirror app = do
    -- allocate state reference
    stRef <- newTVarIO (appInitialState app, 0)
    let applyAction act = atomically $ do
            req <- atomicModifyIORef' stRef $ \(st, revId) ->
                let (st', req) = appApplyAction app act st
                 in ((st', succ revId), req)
            req applyAction

    forkIO $ appInitialRequest app applyAction

    return (handleEvent stRef, handleGetReflection stRef)
  where
    handleEvent stRef ev evRevId = do
        atomicModifyIORef' stRef $ \(st, revId) ->
            case lookupById pos (render st) of
              Nothing -> return () -- ignore event that we cannot locate
              Just evToAct
                | revId /= evRevId -> return () -- ignore event that was too late
                | otherwise        -> do
                    case evToAct ev of
                      Nothing  -> return ()
                      Just act ->
                        let (st', req) = appApplyAction app act st
                         in ((st', succ revId), req)

    handleGetReflection stRef clientRevId = atomicaly $ do
        (st, revId) <- readTVar stRef
        guard (clientRevId /= revId)
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
