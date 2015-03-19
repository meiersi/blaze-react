{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  Platform-independent formulation of the TodoMVC app.
 -}

module Blaze.Core.Examples.Todo
    ( app

      -- NOTE (asayers): This stuff is required for implementing rendering and
      -- request handing. I think it's OK to be exported from here.
    , TodoR
    , TodoA(..)
    , TodoS(..)
    , TodoItemsAction(..)
    , TodoItem(..), tdDone, tdDesc
    , TodoItems
    , EditFocus

    , testTodo
    ) where

import           Prelude hiding (div)

import           Blaze.Core
import qualified Blaze.Core.Service.Store as Store

import           Control.Applicative
import           Control.Lens
                 ( makeLenses, view, traverse, set, ix, allOf , to, _1, _2
                 , _Just, (%=), (.=), preuse, use
                 )
import           Control.Monad
import           Control.Monad.Trans.Maybe        (MaybeT(..), runMaybeT)

import           Data.Aeson      (ToJSON(..), FromJSON(..), (.:), Value(..), object)
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import qualified Data.Text       as T
import           Data.Typeable   (Typeable)

import           Test.QuickCheck

------------------------------------------------------------------------------
-- State representation
------------------------------------------------------------------------------

data TodoItem = TodoItem
    { _tdDone :: !Bool
    , _tdDesc :: !T.Text
    } deriving (Eq, Ord, Show, Read, Typeable)

-- TODO (AS): Use an IntMap so we can persist the identity of the items when
-- new ones are added.
type TodoItems = [TodoItem]

-- | What item is being edited and together with the new text value that it
-- should have.
type EditFocus = Maybe (Int, T.Text)

-- | The state of our todo list editing app.
data TodoS = TodoS
    { _tsNewItemDesc :: !T.Text
    , _tsEditFocus   :: !EditFocus
    , _tsItems       :: !TodoItems
    } deriving (Eq, Show, Typeable)


-- Instances
--------------

instance ToJSON TodoItem where
    toJSON (TodoItem done desc) = object [("done", toJSON done), ("desc", toJSON desc)]

instance FromJSON TodoItem where
    parseJSON (Object v) = TodoItem <$> v .: "done" <*> v .: "desc"
    parseJSON _          = mzero

-- lenses
---------

makeLenses ''TodoItem
makeLenses ''TodoS



------------------------------------------------------------------------------
-- State transitions
------------------------------------------------------------------------------


-- representation
-----------------

-- | Serializable representations of state transitions possible for a list of
-- todo items.
data TodoItemsAction
    = SetItemA    Int Bool
    | DeleteItemA Int
    | SetAllItemsA Bool
      -- ^ If there is one item that is not completed, then set all items to
      -- completed. Otherwise, set all items to incomplete.
    | ClearCompletedA
      -- ^ Remove all completed items.
    deriving (Eq, Ord, Show, Read, Typeable)

-- | Serializable representations of state transitions possible for our todo
-- item management app.
data TodoA
    = TodoItemsActionA TodoItemsAction
    | CreateItemA
    | UpdateNewItemDescA T.Text
    | EditItemA Int
    | UpdateEditTextA T.Text
    | CommitAndStopEditingA
    | ReadFromStoreA (Store.StoreA TodoItems)
      -- ^ The answer from the initial store read.
    deriving (Eq, Ord, Show, Read, Typeable)

-- | The todo app will issue a request to read the 'TodoItems' from some
-- store.
type TodoR = [Store.StoreR TodoItems]

-- instances
------------

instance Arbitrary TodoItem where
instance Arbitrary TodoItemsAction where
instance Arbitrary TodoA           where


-- execution
------------

applyTodoItemsAction :: TodoItemsAction -> TodoItems -> TodoItems
applyTodoItemsAction action items = case action of
    SetItemA itemIdx done -> set (ix itemIdx . tdDone) done items
    DeleteItemA itemIdx   -> map snd $ filter ((itemIdx /=) . fst) $ zip [0..] items
    SetAllItemsA done     -> set (traverse . tdDone) done items
    ClearCompletedA       -> filter (not . view tdDone) items

-- NOTE (meiersi): for production use we'd want to use a more expressive monad
-- that also logs reasons for exceptions. We probably also want to check
-- invariants at specific points to simplify formulating tests.
--
-- TODO (asayers): It would be nice to guarantee that any time tsItems is
-- modified, we also submit a persist-items request...
applyTodoA :: TodoA -> ApplyActionM TodoS TodoR ()
applyTodoA action = case action of
      ReadFromStoreA (Store.ReadA items) ->
          tsItems .= items

      TodoItemsActionA action' -> do
          tsItems %= applyTodoItemsAction action'
          persistItems

      EditItemA itemIdx -> do
          commitAndStopEditing
          persistItems
          discardErrors $ do
              itemDesc <- MaybeT $ preuse (tsItems . ix itemIdx . tdDesc)
              tsEditFocus .= Just (itemIdx, itemDesc)

      CommitAndStopEditingA -> do
          commitAndStopEditing
          persistItems

      UpdateEditTextA newText ->
          tsEditFocus . _Just . _2 .= newText

      CreateItemA -> do
          newItemDesc <- use tsNewItemDesc
          unless (T.null newItemDesc) $ do
              tsItems       %= (TodoItem False newItemDesc :)
              tsNewItemDesc .= ""
              persistItems

      UpdateNewItemDescA newText ->
          tsNewItemDesc .= newText
  where
    discardErrors :: Functor m => MaybeT m () -> m ()
    discardErrors m = fromMaybe () <$> runMaybeT m

    commitAndStopEditing :: ApplyActionM TodoS TodoR ()
    commitAndStopEditing = discardErrors $ do
        (itemIdx, newDesc) <- MaybeT $ use tsEditFocus
        tsEditFocus                   .= Nothing
        tsItems . ix itemIdx . tdDesc .= newDesc

    persistItems :: ApplyActionM TodoS TodoR ()
    persistItems =
        submitRequest . (:[]) . Store.WriteR =<< use tsItems


------------------------------------------------------------------------------
-- Defining and running the app
------------------------------------------------------------------------------


app :: App TodoS TodoA TodoR
app = App
    { appInitialState   = initialState
    , appInitialRequest = [Store.ReadR]
    , appApplyAction    = runApplyActionM . applyTodoA
    }

initialState :: TodoS
initialState = TodoS
    { _tsNewItemDesc = ""
    , _tsEditFocus = Nothing
    , _tsItems = []
    }


------------------------------------------------------------------------------
-- Testing the app
------------------------------------------------------------------------------

wfErrors :: TodoS -> [String]
wfErrors st =
    do guard (not $ allOf (tsEditFocus . _Just . _1) validItemIndex st)
       return $ "Edit focus out of range: " <> show (view tsEditFocus st)
  where
    validItemIndex i = 0 <= i && i < view (tsItems . to length) st

testTodo :: IO ()
testTodo =
    quickCheck (testApp validState (concatMap reqToActs) app)
  where
    validState = null . wfErrors
    reqToActs (Store.WriteR _items) = []
    reqToActs Store.ReadR           =
        [ReadFromStoreA (Store.ReadA (view tsItems $ appInitialState app))]

