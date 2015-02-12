-- | An app that allows to play the client for an arbitrary other app running
-- on the server.
module Blaze2.Core.Examples.AsyncMirror
  ( mirror
  ) where

import Blaze2.Core

data ReactJSEvent = ReactJSEvent -- placeholder

-- | Client state encapsulating server-state 'ss' and view 'v'.
type MirrorS ss view = Maybe (ss, view)

-- | Report that the server failed to return a new state or that there is a
-- new state.
data MirrorA ss view
    = UpdateReflectionA !ss !view
      -- ^ Update the state and view of the reflection that we are
      -- maintaining.
    | HandleEventA !ReactJSEvent
      -- ^ Handle event that was triggered in our view.

data MirrorR ss
    = GetNextServerStateR !(Maybe ss)
    | HandleEventR !ReactJSEvent

-- | Create an app for proxying the session with the given 'SessionId', which
-- is usually chosen randomly.
--
-- We assume that each request for a next server state will be answered with
-- either 'Nothing' or a new server-state.
--
-- TODO (SM): this is more like mirroring a session over an unreliable channel
-- => consider a rename.
mirror :: App (MirrorS ss view) (MirrorA ss view) [MirrorR ss]
mirror = App
    { appInitialState   = Nothing
    , appInitialRequest = [GetNextServerStateR Nothing]
    , appApplyAction    = \act -> runApplyActionM $ do
        case act of
          HandleEventA ev           -> submitRequest [HandleEventR ev]
          UpdateReflectionA ss view -> do
            writeState (Just (ss, view))
            submitRequest [GetNextServerStateR (Just ss)]
    }


