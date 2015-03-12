{-# LANGUAGE OverloadedStrings #-}

module Blaze.ReactJS.Examples.Todo
    ( renderState
    , handleRequest
    ) where

import           Blaze.Core.Examples.Todo
import           Blaze.ReactJS.Base
import qualified Blaze.ReactJS.Service.Store as Store

import           Control.Lens    (folded, to, sumOf)
import           Control.Monad

import           Data.Foldable   (foldMap)
import           Data.Monoid     ((<>), mempty)
import qualified Data.Text       as T

import           Prelude hiding (div)

import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Event.Keycode             as Keycode
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A

------------------------------------------------------------------------------
-- Handling
------------------------------------------------------------------------------

handleRequest :: T.Text -> TodoR -> (TodoA -> IO ()) -> IO ()
handleRequest storeName reqs channel = forM_ reqs $
    Store.handleRequest storeName defaultItems (channel . ReadFromStoreA)
  where
    defaultItems =
      [ TodoItem True "Implement persistent storage"
      , TodoItem False "???"
      , TodoItem False "Profit!"
      ]


------------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------------

renderState :: TodoS -> WindowState TodoA
renderState state = WindowState
    { _wsBody = renderBody state
    , _wsPath = ""
    }

renderBody :: TodoS -> H.Html TodoA
renderBody (TodoS newItemDesc mbEditFocus items) = do
    -- app
    H.section H.! A.id "todoapp" $
      H.div $ do
        -- header
        H.header H.! A.id "header" $ do
          H.h1 "todos"
          H.input H.! A.id "new-todo"
                  H.! A.placeholder "What needs to be done?"
                  H.! A.autofocus True
                  H.! A.value (H.toValue newItemDesc)
                  H.! E.onValueChange UpdateNewItemDescA
                  H.! E.onKeyDown [Keycode.enter] CreateItemA

        -- items
        unless (null items) $ do
            H.section H.! A.id "main" $ do
              H.input
                  H.! A.type_ "checkbox"
                  H.! A.id "toggle-all"
                  H.! A.checked (numTodo == 0)
                  H.! E.onCheckedChange (TodoItemsActionA . SetAllItemsA)
              H.label H.! A.for "toggle-all" $ "Mark all as complete"
              H.ul H.! A.id "todo-list" $
                foldMap (renderTodoItem mbEditFocus) $ zip [0..] items

            -- item footer
            H.footer H.! A.id "footer" $ do
              H.span H.! A.id "todo-count" $ do
                H.strong (H.toHtml numTodo)
                (if numTodo == 1 then " item" else " items") <> " left"

              unless (numCompleted == 0) $
                H.button
                    H.! A.id "clear-completed"
                    H.! E.onClick' (TodoItemsActionA ClearCompletedA)
                    $ "Clear completed (" <> H.toHtml numCompleted <> ")"

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
    numTodo      = sumOf (folded . tdDone . to not . to fromEnum) items
    numCompleted = length items - numTodo


renderTodoItem :: EditFocus -> (Int, TodoItem) -> H.Html TodoA
renderTodoItem mbEditFocus (itemIdx, TodoItem done desc) = do
   H.li H.! itemClass
        H.! A.key (H.toValue itemIdx)
     $ do H.div H.! A.class_ "view" $ do
            H.input
                H.! A.type_ "checkbox"
                H.! A.class_ "toggle"
                H.! A.checked done
                H.! E.onCheckedChange (TodoItemsActionA . SetItemA itemIdx)
            H.label
                H.! E.onDoubleClick' (EditItemA itemIdx)
                $ H.toHtml desc
            H.button
                H.! A.class_ "destroy"
                H.! E.onClick' (TodoItemsActionA (DeleteItemA itemIdx))
                $ mempty
          case mbEditFocus of
           Just (focusIdx, focusText)
               | focusIdx == itemIdx ->
                   H.input H.! A.class_ "edit"
                           H.! A.value (H.toValue focusText)
                           H.! A.autofocus True
                           H.! E.onValueChange UpdateEditTextA
                           H.! E.onBlur CommitAndStopEditingA
                           H.! E.onKeyDown [Keycode.enter] CommitAndStopEditingA
               | otherwise -> mempty
           Nothing         -> mempty
  where
    itemClass
      | isBeingEdited = A.class_ "editing"
      | done          = A.class_ "completed"
      | otherwise     = mempty

    isBeingEdited = Just itemIdx == fmap fst mbEditFocus



