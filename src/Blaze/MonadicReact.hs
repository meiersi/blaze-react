{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Blaze.MonadicReact
    ( App(..)

    ) where
    -- ( App(..)
    -- , noRequest
    -- , Ref(..)
    -- , mapRef
    -- , SetPathR
    -- , PathChangedA(..)
    -- , withPath

    --   -- * Accessing key-value stores and JSON API's
    -- , TextKVStore
    -- , Async
    -- , AsyncRef(..)
    -- ) where

import           Control.Lens
                 ( Prism', review, over, _2, anyOf, traverse
                 , Iso', view, from, preview
                 )
import           Control.Monad

import           Data.Foldable        (Foldable())
import qualified Data.HashMap.Strict  as HMS
import           Data.Monoid
import           Data.Profunctor  (Profunctor(dimap))
import qualified Data.Text        as T
import           Data.Traversable (Traversable())

import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import           Control.Monad.Trans.State  (State, execState, runState, put, get, modify)

import           Prelude hiding (read)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data App st act req = App
    { appInitialState   :: !st
    , appApplyAction    :: !(act -> st -> st)
    , appGetNextRequest :: !(st -> (st, req))
    }

instance Functor (App st act) where
    fmap f (App st0 apply next) = App st0 apply (over _2 f . next)

instance Profunctor (App st) where
    dimap f g (App st0 apply next) =
        App st0 (\act st -> apply (f act) st) (\st -> over _2 g (next st))


------------------------------------------------------------------------------
-- Async API requests
------------------------------------------------------------------------------

data ApiR api = forall a b. ApiR (api a b) a

data ApiA api = forall a b. ApiA (api a b) a b


data AsyncResult a
    = AsyncResult a
    | AsyncError
    deriving (Eq, Show, Functor, Foldable, Traversable)

type UserId = T.Text

data User = User
    { uName :: T.Text
    }
    deriving (Eq, Show)

data Github a b where
    ListUsers  :: Github ()     (AsyncResult [UserId])
    LookupUser :: Github UserId (AsyncResult (Maybe User))

data ConsoleA
    = ConsoleUserInput !T.Text

data ConsoleApp st act = ConsoleApp
    { caApp    :: App st (Either ConsoleA act) ((act -> IO ()) -> IO ())
    , caRender :: st -> (Maybe T.Text)
    }

-- | We can execute console apps by starting with the initial state, fetching
-- the initial request, and applying actions taken from a TChan, and rendering
-- whenever the TChan is empty. We interpret an empty Text as the signal to
-- quit
--
-- TODO (SM): use the console app to build a console-based github user browser
-- with async loading of users.
runConsoleApp :: ConsoleApp st act -> IO ()
runConsoleApp = error "runConsoleApp"


pureEvalGithubApiR :: HMS.HashMap UserId User -> ApiR Github -> ApiA Github
pureEvalGithubApiR users req = case req of
    ApiR ListUsers  ()     -> ApiA ListUsers () (AsyncResult (HMS.keys users))
    ApiR LookupUser userId ->
      ApiA LookupUser userId (AsyncResult (HMS.lookup userId users))

runAppPure :: (req -> Bool) -> (req -> [act]) -> App st act req -> [st]
runAppPure isEmptyReq reqToActs app =
    go (appInitialState app) []
  where
    go st []         = case appGetNextRequest app st of
      (st', req)
        | isEmptyReq req -> []
        | otherwise      -> st' : go st' (reqToActs req)
    go st (act:acts) = case appApplyAction app act st of
      st' -> st' : go st' acts

githubTest :: [GithubUserListS]
githubTest = runAppPure null (map (pureEvalGithubApiR users)) githubUserList
  where
    users = HMS.fromList
      [ ("x1", User "Simon Meier")
      , ("x2", User "Alex Sayers")
      , ("x3", User "Carl Baatz")
      ]


data Delayed a
    = Unfetched
    | Fetching
    | Fetched a
    deriving (Eq, Show, Functor, Foldable, Traversable)

type DelayedAsync a = Delayed (AsyncResult a)

type GithubUserListS = DelayedAsync [(UserId, DelayedAsync (Maybe User))]

isUnfetched :: Delayed a -> Bool
isUnfetched Unfetched = True
isUnfetched _         = False


githubUserList :: App GithubUserListS (ApiA Github) [ApiR Github]
githubUserList = App
    { appInitialState   = Unfetched
    , appApplyAction    = execState . apply
    , appGetNextRequest = swap . runState (execWriterT getNextRequest)
    }
  where
    swap (x,y) = (y,x)

    apply :: ApiA Github -> State GithubUserListS ()
    apply act = case act of
      ApiA ListUsers () AsyncError            -> put (Fetched AsyncError)

      ApiA ListUsers () (AsyncResult userIds) -> do
        let users = [(userId, Unfetched) | userId <- userIds]
        put (Fetched (AsyncResult users))

      ApiA LookupUser userId errOrUser -> do
          let replace x@(userId', _)
                | userId == userId' = (userId, Fetched errOrUser)
                | otherwise         = x
          modify (fmap (fmap (map replace)))

    getNextRequest :: WriterT [ApiR Github] (State GithubUserListS) ()
    getNextRequest = do
        users <- lift get
        case users of
          Unfetched          -> do tell [ApiR ListUsers ()]
                                   lift $ put Fetching
          Fetching           -> return ()
          Fetched errOrUsers ->
            case errOrUsers of
              AsyncError        -> return ()
              AsyncResult users
                -- Only fetch users if there is something to fetch, to avoid
                -- reallocating the users list on every 'getNextRequest' call.
                | anyOf (traverse . _2) isUnfetched users -> do
                    newUsers <- forM users $ \x@(userId, user) ->
                        if isUnfetched user
                          then do tell [ApiR LookupUser userId]
                                  return (userId, Fetching)
                          else return x
                    lift $ put (Fetched (AsyncResult newUsers))
                | otherwise -> return ()



{-

data App st m act req = App
    { appInitialState   :: !st
    , appApplyAction    :: !(act -> st -> m st)
    , appGetNextRequest :: !(st -> m (st, req))
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

-- QUESTION (SM): how to support asynchronous callbacks. One option is a type
-- that takes some input 'i' and a callback to convert the output 'o' of the
-- asynch request to a request of the inner application.
type Async m d i req = (d -> req) -> i -> m ()

type Async a b = a -> b

type UserId = T.Text



{-
data GithubApiR a
    = ListUsersR (AsyncResult [UserId] -> a)
    | LookupUserR UserId (AsyncResult User

data GithubApiA
    = ListUsersA (AsyncResult [UserId]

evalGithubApiR
    :: GithubApi.Handle -> GithubApiR act -> IO act -> IO ()
-}
evalGithubApiR



withAjax
    ::
    (    Async (WithAjaxT m) Url Json req
      -> App st (WithAjaxT m) act req
    )
    ->
    App (AjaxR req, st) m (AjaxA req act) (AjaxR req, req)

data AsyncRef m a r = AsyncRef
    { arRead  :: (a -> m ()) -> m ()
      -- ^ Each read will call the call-back at-most once.
    , arWrite :: a -> m ()
      -- ^ No notification about whether the write was successful or not is
      -- given.
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
-}
