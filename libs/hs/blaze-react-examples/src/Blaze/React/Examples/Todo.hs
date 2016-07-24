{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  Platform-independent formulation of the TodoMVC app.
 -}

module Blaze.React.Examples.Todo
    ( app
    , render
    ) where

import           Prelude hiding (div)

import           Blaze.React.Core
import qualified Blaze.React.Html5                as H
import qualified Blaze.React.Html5.Attributes     as A
import qualified Blaze.React.Html5.Event          as E
import qualified Blaze.React.Html5.Event.Keycode  as Keycode

import           Control.Lens
                 ( makeLenses, view, set, ix, _2, sumOf, folded, to
                 , _Just, (%=), (.=), preuse, use
                 )
import           Control.Monad
import           Control.Monad.Trans.Maybe        (MaybeT(..), runMaybeT)

import           Data.Aeson                       (ToJSON, FromJSON)
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import qualified Data.Text       as T
import           Data.Typeable   (Typeable)

import           GHC.Generics   (Generic)

import           Test.QuickCheck

------------------------------------------------------------------------------
-- Helper functions (TODO (SM): move them)
------------------------------------------------------------------------------

unless_ :: Monoid m => Bool -> m -> m
unless_ True _ = mempty
unless_ False  m = m


------------------------------------------------------------------------------
-- State representation
------------------------------------------------------------------------------

data TodoItem = TodoItem
    { _tdDone :: !Bool
    , _tdDesc :: !T.Text
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

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
    } deriving (Eq, Show, Generic, Typeable)

type TodoR = ()

-- Instances
--------------

instance FromJSON TodoItem
instance FromJSON TodoS
instance ToJSON TodoItem
instance ToJSON TodoS

{-
instance ToJSON TodoItem where
    toJSON (TodoItem done desc) = object [("done", toJSON done), ("desc", toJSON desc)]

instance FromJSON TodoItem where
    parseJSON (Object v) = TodoItem <$> v .: "done" <*> v .: "desc"
    parseJSON _          = mzero
-}

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
    | NoopA
      -- ^ A noop to return for some events.
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
    deriving (Eq, Ord, Show, Read, Typeable)


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
    NoopA                 -> items

-- NOTE (meiersi): for production use we'd want to use a more expressive monad
-- that also logs reasons for exceptions. We probably also want to check
-- invariants at specific points to simplify formulating tests.
--
-- TODO (asayers): It would be nice to guarantee that any time tsItems is
-- modified, we also submit a persist-items request...
applyTodoA :: TodoA -> ApplyActionM TodoS () ()
applyTodoA action = case action of
      TodoItemsActionA action' -> do
          tsItems %= applyTodoItemsAction action'

      EditItemA itemIdx -> do
          commitAndStopEditing
          discardErrors $ do
              itemDesc <- MaybeT $ preuse (tsItems . ix itemIdx . tdDesc)
              tsEditFocus .= Just (itemIdx, itemDesc)

      CommitAndStopEditingA -> do
          commitAndStopEditing

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
    , appInitialRequest = ()
    , appApplyAction    = runApplyActionM . applyTodoA
    }

initialState :: TodoS
initialState = TodoS
    { _tsNewItemDesc = ""
    , _tsEditFocus = Nothing
    , _tsItems = []
    }

{-

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
    reqToActs _ = []

-}


------------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------------

render :: TodoS -> H.Html (E.EventHandler TodoA)
render (TodoS newItemDesc mbEditFocus items) = mconcat
    -- app
  [ H.section H.! A.id "todoapp" $
      H.div $ mconcat
          -- header
        [ H.header H.! A.id "header" $ mconcat
            [ H.h1 "todos"
            , H.input H.! A.id "new-todo"
                      H.! A.placeholder "What needs to be done?"
                      H.! A.autofocus True
                      H.! A.value (H.toValue newItemDesc)
                      H.! E.onValueChange UpdateNewItemDescA
                      H.! E.onKeyDown' [Keycode.enter] CreateItemA
            ]

          -- items
        , unless_ (null items) $ mconcat
            [ H.section H.! A.id "main" $ mconcat
                [ H.input
                      H.! A.type_ "checkbox"
                      H.! A.id "toggle-all"
                      H.! A.checked (numTodo == 0)
                      H.! E.onCheckedChange (TodoItemsActionA . SetAllItemsA)
                , H.label H.! A.for "toggle-all" $ "Mark all as complete"
                , H.ul H.! A.id "todo-list" $
                    foldMap (renderTodoItem mbEditFocus) $ zip [0..] items
                ]
            , itemFooter
            ]
        ]
    -- app footer
  , H.footer H.! A.id "info" $ mconcat
      [ H.p "Double-click to edit a todo"
      , H.p $ mconcat
          [ "Created by "
          , H.a H.! A.href "https://github.com/meiersi" $ "Simon Meier"
          , " based on the "
          , H.a H.! A.href "https://github.com/facebook/flux" $ "flux"
          , " TodoMVC example by "
          , H.a H.! A.href "http://facebook.com/bill.fisher.771" $ "Bill Fisher"
          ]
      , H.p $ mconcat
          [ "A (future;-) part of "
          , H.a H.! A.href "http://todomvc.com" $ "TodoMVC"
          ]
      ]
  ]
  where
    numTodo      = sumOf (folded . tdDone . to not . to fromEnum) items
    numCompleted = length items - numTodo

    itemFooter =
        H.footer H.! A.id "footer" $ mconcat
          [ H.span H.! A.id "todo-count" $ mconcat
              [ H.strong (H.toHtml numTodo)
              , (if numTodo == 1 then " item" else " items") <> " left"
              ]

          , unless_ (numCompleted == 0) $
              H.button
                  H.! A.id "clear-completed"
                  H.! E.onClick' (TodoItemsActionA ClearCompletedA)
                  $ "Clear completed (" <> H.toHtml numCompleted <> ")"
          ]

renderTodoItem
    :: EditFocus -> (Int, TodoItem) -> H.Html (E.EventHandler TodoA)
renderTodoItem mbEditFocus (itemIdx, TodoItem done desc) = do
   H.li H.! itemClass
        H.! A.key (H.toValue itemIdx)
     $ mconcat
         [ H.div H.! A.class_ "view" $ mconcat
             [ H.input
                   H.! A.type_ "checkbox"
                   H.! A.class_ "toggle"
                   H.! A.checked done
                   H.! E.onCheckedChange (TodoItemsActionA . SetItemA itemIdx)
             , H.label
                   H.! E.onDoubleClick' (EditItemA itemIdx)
                   $ H.toHtml desc
             , H.button
                   H.! A.class_ "destroy"
                   H.! E.onClick' (TodoItemsActionA (DeleteItemA itemIdx))
                   $ mempty
             ]
         , case mbEditFocus of
            Just (focusIdx, focusText)
                | focusIdx == itemIdx ->
                    H.input H.! A.class_ "edit"
                            H.! A.value (H.toValue focusText)
                            H.! A.autofocus True
                            H.! E.onValueChange UpdateEditTextA
                            -- H.! E.onBlur CommitAndStopEditingA
                            H.! E.onKeyDown' [Keycode.enter] CommitAndStopEditingA
                | otherwise -> mempty
            Nothing         -> mempty
         ]
  where
    itemClass
      | isBeingEdited = A.class_ "editing"
      | done          = A.class_ "completed"
      | otherwise     = mempty

    isBeingEdited = Just itemIdx == fmap fst mbEditFocus

