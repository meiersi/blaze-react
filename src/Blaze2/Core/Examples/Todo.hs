{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  Platform-independent formulation of the TodoMVC app.
 -}

module Blaze2.Core.Examples.Todo
    ( app

    , TodoR(..)
    , TodoA(..)
    , TodoS(..)
    , TodoItemsAction(..)
    , TodoItem(..), tdDone, tdDesc
    , TodoItems
    , EditFocus
    ) where

import           Prelude hiding (div)

import           Blaze2.Core
import qualified Blaze2.Core.Service.Store as Store

import           Control.Applicative
import           Control.Lens
                 ( makeLenses, view, traverse, folded, set, ix, allOf
                 , to, _1, _2, _Just, sumOf, (%=), (.=), preuse, use
                 )
import           Control.Monad
import           Control.Monad.Trans.Maybe        (MaybeT(..), runMaybeT)

import           Data.Foldable   (foldMap)
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>), mempty)
import qualified Data.Text       as T
import           Data.Typeable   (Typeable)

import           GHC.Generics (Generic)

import           Test.QuickCheck

------------------------------------------------------------------------------
-- State representation
------------------------------------------------------------------------------

data TodoItem = TodoItem
    { _tdDone :: !Bool
    , _tdDesc :: !T.Text
    } deriving (Eq, Ord, Show, Read)

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
    } deriving (Eq, Show)



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
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

-- | The todo app will issue a request to read the 'TodoItems' from some
-- store.
type TodoR = [Store.StoreR TodoItems]

-- instances
------------

instance Arbitrary TodoItem where
instance Arbitrary v => Arbitrary (Store.StoreA v)   where
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
applyTodoA :: TodoA -> ApplyActionM TodoS TodoR ()
applyTodoA action = case action of
      ReadFromStoreA (Store.ReadA items) ->
          tsItems .= items

      TodoItemsActionA action' ->
          tsItems %= applyTodoItemsAction action'

      EditItemA itemIdx -> do
          commitAndStopEditing
          discardErrors $ do
              itemDesc <- MaybeT $ preuse (tsItems . ix itemIdx . tdDesc)
              tsEditFocus .= Just (itemIdx, itemDesc)

      CommitAndStopEditingA -> commitAndStopEditing

      UpdateEditTextA newText ->
          tsEditFocus . _Just . _2 .= newText

      CreateItemA -> do
          newItemDesc <- use tsNewItemDesc
          unless (T.null newItemDesc) $ do
              tsItems       %= (TodoItem False newItemDesc :)
              tsNewItemDesc .= ""

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


testApp
    :: (Show act)
    => (st -> Bool)
    -> (req -> [act])
    -> App st act req
    -> [act]
    -> Bool
testApp validState reqToActs app =
    go (appInitialState app)
  where
    go st acts0 =
        validState st &&
        case acts0 of
          []         -> True
          (act:acts) ->
            case appApplyAction app act st of
              (st', req) -> go st' (reqToActs req ++ acts)











