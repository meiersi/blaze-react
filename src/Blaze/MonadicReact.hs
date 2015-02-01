{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Blaze.MonadicReact
    ( App(..)
    , noRequest
    , Ref(..)
    , mapRef
    , SetPathR
    , PathChangedA(..)
    , withPath

      -- * Accessing key-value stores and JSON API's
    , TextKVStore
    , Async
    , AsyncRef(..)
    ) where

import           Control.Lens
                 ( Prism', review, over, _2
                 , Iso', view, from, preview
                 )

import qualified Data.Text as T
import           Data.Profunctor (Profunctor(dimap))
import           Data.Monoid

import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

import           Prelude hiding (read)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data App st m act req = App
    { appInitialState   :: !st
    , appApplyAction    :: !(act -> st -> m st)
    , appGetNextRequest :: !(st -> (st, req))
      -- Called at any point in time to gather the outstanding requests.
    }

data Ref m a = Ref
    { rRead  :: !(m a)
    , rWrite :: !(a -> m ())
    }

mapRef :: Functor m => Iso' a b -> Ref m a -> Ref m b
mapRef iso (Ref read write) =
    Ref (fmap (view iso) read) (write . view (from iso))

type SetPathR = Last

setPathR :: loc -> SetPathR loc
setPathR = Last . Just

noRequest :: Monoid req => req
noRequest = mempty

data PathChangedA act
    = PathChanged !T.Text
    | PathChangedInner !act


type WithPathT loc m = WriterT (SetPathR loc) (ReaderT loc m)


-- | Wrap an application such that it can read and write the current path
-- during action application.
withPath
    :: (Monad m, Monoid req)
    => (Prism' T.Text loc)
       -- ^ How to extract the current location.
    -> (    Ref (WithPathT loc m) loc
         -> App st (WithPathT loc m) act req
       )
       -- ^ How to construct an app from a reference to the current location.
    -> loc
       -- ^ The initial location.
    -> App (loc, SetPathR loc, st) m (PathChangedA act) (SetPathR T.Text, req)
       -- ^ The app that asynchronously tracks the current location.
withPath textToLoc mkApp initialLoc =
    App
      { appInitialState   = (initialLoc, noRequest, appInitialState innerApp)
      , appApplyAction    = \act (loc, outstandingReq, innerSt0) -> do
          case act of
            PathChanged newPath -> return $!
                case preview textToLoc newPath of
                  -- Failed to parse path: if there is no outstanding request
                  -- then set the path to the current location.
                  Nothing     -> (loc, setPathR loc <> outstandingReq, innerSt0)
                  -- New location received: store it
                  Just newLoc -> (newLoc, outstandingReq, innerSt0)

            PathChangedInner innerAct -> do
                let m = appApplyAction innerApp innerAct innerSt0
                (innerSt, req) <- runReaderT (runWriterT m) loc
                return (loc, outstandingReq <> req, innerSt)

      , appGetNextRequest = \(loc, outstandingReq, innerSt0)  ->
            case appGetNextRequest innerApp innerSt0 of
              (innerSt, innerReq) ->
                ( (loc, noRequest, innerSt)
                , ( Last $ fmap (review textToLoc) $ getLast outstandingReq
                  , innerReq
                  )
                )
      }
  where
    innerApp = mkApp $ Ref { rRead  = lift ask
                           , rWrite = tell . setPathR
                           }


-- | A synchronous 'T.Text' key value store can be represented faithfully
-- using the following type.
type TextKVStore m = T.Text -> m (Ref m (Maybe T.Text))

type Async m d i o = i -> m (d -> m o)

data AsyncRef m a r = AsyncRef
    { arRead  :: m (a -> m r)
      -- ^ Each read will call the call-back at-most once.
    , arWrite :: a -> m ()
      -- ^ No notification of 
    }


-- NOTE (SM): RUD applications are composed from little read-write reference
-- applications.
--
-- If we want to combine rendering and request generation, then we can
-- consider using a WriterT in the render function and caching both the
-- resulting requests and the resulting view.



instance Functor m => Functor (App st m act) where
    fmap f (App st0 apply nextReq) = App st0 apply (over _2 f . nextReq)

instance Functor m => Profunctor (App st m) where
    dimap f g (App st0 apply nextReq) =
        App st0 (\act st -> apply (f act) st) (\st -> over _2 g (nextReq st))

