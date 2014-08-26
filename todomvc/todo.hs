{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}

module Main where

import           Prelude hiding (div)


import           Control.Lens
                 ( makeLenses, view, preview, traverse, folded, set, over, ix
                 , to, _2, _Just, sumOf
                 )
import           Control.Monad (void)

import           Data.Foldable   (foldMap)
import           Data.Monoid     ((<>), mempty)
import qualified Data.Text       as T

import qualified Html                           as H
import qualified Html.Attributes                as A
-- import qualified Html.Renderer.Pretty           as H.Pretty
import qualified Html.Renderer.String           as H.String

-- import           GHCJS.VDOM          as VD
-- import           GHCJS.VDOM.QQ       as VD
-- import           GHCJS.Foreign
-- import           GHCJS.Foreign.QQ
-- import           GHCJS.Types


------------------------------------------------------------------------------
-- Application state representation
------------------------------------------------------------------------------

data TodoItem = TodoItem
    { _tdDone :: !Bool
    , _tdDesc :: !T.Text
    } deriving (Eq, Ord, Show)

type TodoItems = [TodoItem]

-- | What item is being edited and together with the new text value that it
-- should have.
type EditFocus = Maybe (Int, T.Text)

-- | The state of our todo list editing app.
data TodoState = TodoState
    { _tsEditFocus :: !EditFocus
    , _tsItems     :: !TodoItems
    }


-- lenses
---------

makeLenses ''TodoItem
makeLenses ''TodoState


------------------------------------------------------------------------------
-- State transitions
------------------------------------------------------------------------------


-- representation
-----------------

-- | Serializable representations of state transitions possible for a list of
-- todo items.
data TodoItemsAction
    = CreateItemA T.Text
    | ToggleItemA Int
    | DeleteItemA Int
    | MarkAllAsCompleteA
    deriving (Eq, Ord, Show, Read)

-- | Serializable representations of state transitions possible for our todo
-- item management app.
data TodoAction
    = TodoItemsActionA TodoItemsAction
    | EditItemA Int
    | UpdateEditTextA T.Text
    | CommitAndStopEditingA
    deriving (Eq, Ord, Show, Read)


-- execution
------------

applyTodoItemsAction :: TodoItemsAction -> [TodoItem] -> [TodoItem]
applyTodoItemsAction action = case action of
    CreateItemA desc    -> (TodoItem False desc :)
    ToggleItemA itemIdx -> over (ix itemIdx . tdDone) not
    DeleteItemA itemIdx -> map snd . filter ((itemIdx /=) . fst) . zip [0..]
    MarkAllAsCompleteA  -> set (traverse . tdDone) True

applyTodoAction :: TodoAction -> TodoState -> TodoState
applyTodoAction action st = case action of
    TodoItemsActionA action' -> over tsItems (applyTodoItemsAction action') st

    EditItemA itemIdx ->
        case preview (tsItems . ix itemIdx) st of
          Nothing   -> st
          Just (TodoItem _done desc) ->
              set tsEditFocus (Just (itemIdx, desc))
            $ applyTodoAction CommitAndStopEditingA
            $ st

    CommitAndStopEditingA ->
        case view tsEditFocus st of
          Nothing                 -> st
          Just (itemIdx, newDesc) ->
             set tsEditFocus Nothing
           $ set (tsItems . ix itemIdx . tdDesc) newDesc
           $ st

    UpdateEditTextA newText -> set (tsEditFocus . _Just . _2) newText st


------------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------------

-- | Serializable references to event handlers.
data TodoEventHandler
    = CreateItemEH
      -- ^ Raised on an input element whose text should be used to create a
      -- new item at the front of the list.
    | ToggleItemEH Int        -- ^ Toggle the @i@-th item
    | DeleteItemEH Int        -- ^ Delete the @i@-th item
    | EditItemEH Int          -- ^ Start editing @i@-th item.
    | EditInputEH             -- ^ Handle an event from the edit input field.
    | MarkAllAsCompleteEH     -- ^
    deriving (Eq, Ord, Show, Read)


renderTodoState :: TodoState -> H.Html TodoEventHandler
renderTodoState (TodoState mbEditFocus items) = do
    -- app
    H.section H.! A.id "todoapp" $
      H.div $ do
        -- header
        H.header H.! A.id "header" $ do
          H.h1 "todos"
          H.input H.! A.id "new-todo"
                  H.! A.placeholder "What needs to be done?"
                  H.! A.value mempty
                  H.! H.onEvent CreateItemEH

        -- items
        H.section H.! A.id "main" $ do
          H.input H.! A.id "toggle-all"
                  H.! A.type_ "checkbox"
                  H.! H.onEvent MarkAllAsCompleteEH
          H.label H.! A.for "toggle-all" $ "Mark all as complete"
          H.ul H.! A.id "todo-list" $
            foldMap (renderTodoItem mbEditFocus) $ zip [0..] items

        -- item footer
        H.footer H.! A.id "footer" $
          H.span H.! A.id "todo-count" $ do
            H.strong (H.toHtml itemsLeftToDo)
            H.span $ (if itemsLeftToDo == 1 then " item" else " items") <> " left"

    -- app footer
    H.footer H.! A.id "info" $ do
      H.p "Double-click to edit a todo"
      H.p $ do
        void $ "Created by "
        H.a H.! A.href "https://github.com/meiersi" $ "Simon Meier"
        void $ " based on the "
        H.a H.! A.href "https://github.com/facebook/flux" $ "flux"
        void $ " TodoMVC example by "
        H.a H.! A.href "http://facebook.com/bill.fisher.771" $ "Bill Fisher"

      H.p $ do
        void $ "A (future;-) part of "
        H.a H.! A.href "http://todomvc.com" $ "TodoMVC"
  where
    itemsLeftToDo = sumOf (folded . tdDone . to not . to fromEnum) items


renderTodoItem :: EditFocus -> (Int, TodoItem) -> H.Html TodoEventHandler
renderTodoItem mbEditFocus (itemIdx, TodoItem done desc) = do
   H.li H.! (if done then A.class_ "completed" else mempty) $ do
     H.div H.! A.class_ "view" $ do
        H.input H.! A.class_ "toggle"
                H.! A.type_ "checkbox"
                H.! H.onEvent (ToggleItemEH itemIdx)
        H.label H.! H.onEvent (EditItemEH itemIdx) $ H.toHtml desc
        H.button H.! A.class_ "destroy"
                 H.! H.onEvent (DeleteItemEH itemIdx)
                 $ mempty
     case mbEditFocus of
      Just (focusIdx, focusText)
          | focusIdx == itemIdx ->
              H.input H.! A.class_ "edit"
                      H.! A.value (H.toValue focusText)
                      H.! H.onEvent EditInputEH
          | otherwise -> mempty
      Nothing         -> mempty



-- Testing the renderer
-----------------------

-- test :: IO ()
-- test = putStrLn $ H.Pretty.renderHtml $ render q0

testCompact :: IO ()
testCompact = putStrLn $ H.String.renderHtml $ render q0

q0 :: TodoState
q0 =
    TodoState (Just (1, "OLD")) (items ++ items)
  where
    items = [TodoItem True "DoNe", TodoItem False "Woaaaaah!"]

