{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-
  Runtime-independent formulation of the TodoMVC app.
 -}

module TodoApp
    (
      -- * Generic infrastructure (TODO (SM): Move)
      App(..)
    , DOMEvent(..)

      -- * Our app
    , TodoEventHandler(..)
    , todoApp
    ) where

import           Prelude hiding (div)

import           Control.Applicative
import           Control.Lens
                 ( makeLenses, view, preview, traverse, folded, set, over, ix
                 , to, _1, _2, _Just, sumOf, andOf
                 )
import           Control.Monad

import           Data.Foldable   (foldMap)
import           Data.Monoid     ((<>), mempty)
import qualified Data.Text       as T
import           Data.Time       (UTCTime)

import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A


------------------------------------------------------------------------------
-- Generic blaze-vdom application types and functions (TO BE MOVED)
------------------------------------------------------------------------------

-- | An incomplete list of 'DOMEvent's.
--
-- It has to be extended or revamped to satisfy more use-cases.
data DOMEvent
    = OnClick
    | OnDoubleClick
    | OnTextInputChange !T.Text
      -- ^ A text value was changed to the given new value.
    | OnBlur
      -- ^ An input element loses focus.
    | OnMouseOver
    deriving (Show)

-- FIXME (AS): I think 'eventHandler' is a misnomer, because it's not like
-- objects of this type contain logic for handling events. These are more like
-- DOM markers.
data App state action eventHandler = App
    { appInitialState    :: state
    , appInitialRequests :: [IO action]
    , appApplyAction     :: action -> state -> (state, [IO action])
    , appRender          :: state -> H.Html eventHandler
    , appHandleEvent     :: UTCTime -> DOMEvent -> eventHandler -> Maybe action
      -- ^ How to translate an event tagged with the time at which it occurred
      -- to an action. No access to the application state on purpose.
    }


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
    { _tsNewItemDesc :: !T.Text
    , _tsEditFocus   :: !EditFocus
    , _tsItems       :: !TodoItems
    } deriving Show


-- lenses
---------

makeLenses ''TodoItem
makeLenses ''TodoState


-- queries
----------

allItemsDone :: TodoItems -> Bool
allItemsDone = andOf (traverse . tdDone)


------------------------------------------------------------------------------
-- State transitions
------------------------------------------------------------------------------


-- representation
-----------------

-- | Serializable representations of state transitions possible for a list of
-- todo items.
data TodoItemsAction
    = ToggleItemA Int
    | DeleteItemA Int
    | ToggleAllItemsA
      -- ^ If there is one item that is not completed, then set all items to
      -- completed. Otherwise, set all items to incomplete.
    | ClearCompletedA
      -- ^ Remove all completed items.
    deriving (Eq, Ord, Show, Read)

-- | Serializable representations of state transitions possible for our todo
-- item management app.
data TodoAction
    = TodoItemsActionA TodoItemsAction
    | CreateItemA
    | UpdateNewItemDescA T.Text
    | EditItemA Int
    | UpdateEditTextA T.Text
    | CommitAndStopEditingA
    deriving (Eq, Ord, Show, Read)


-- execution
------------

applyTodoItemsAction :: TodoItemsAction -> [TodoItem] -> [TodoItem]
applyTodoItemsAction action items = case action of
    ToggleItemA itemIdx -> over (ix itemIdx . tdDone) not items
    DeleteItemA itemIdx -> map snd $ filter ((itemIdx /=) . fst) $ zip [0..] items
    ToggleAllItemsA     -> set (traverse . tdDone) (not (allItemsDone items)) items
    ClearCompletedA     -> filter (not . view tdDone) items

applyTodoAction :: TodoAction -> TodoState -> (TodoState, [IO TodoAction])
applyTodoAction action st = case action of
    TodoItemsActionA action' -> noRequests $ over tsItems (applyTodoItemsAction action') st

    EditItemA itemIdx ->
        case preview (tsItems . ix itemIdx) st of
          Nothing                    -> noRequests st
          Just (TodoItem _done desc) ->
              set (_1 . tsEditFocus) (Just (itemIdx, desc))
            $ applyTodoAction CommitAndStopEditingA
            $ st

    CommitAndStopEditingA ->
        case view tsEditFocus st of
          Nothing                 -> noRequests $ st
          Just (itemIdx, newDesc) -> noRequests $
             set tsEditFocus Nothing
           $ set (tsItems . ix itemIdx . tdDesc) newDesc
           $ st

    UpdateEditTextA newText -> noRequests $ set (tsEditFocus . _Just . _2) newText st

    CreateItemA
        | T.null (view tsNewItemDesc st) -> noRequests st
        | otherwise                      -> noRequests $
              over tsItems (TodoItem False (view tsNewItemDesc st) :)
            $ set tsNewItemDesc "" st
    UpdateNewItemDescA newText -> noRequests $ set tsNewItemDesc newText st
  where
    noRequests x = (x, [])


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
    | ToggleAllEH             -- ^
    | ClearCompletedEH
    deriving (Eq, Ord, Show, Read)


renderTodoState :: TodoState -> H.Html TodoEventHandler
renderTodoState (TodoState newItemDesc mbEditFocus items) = do
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
                  H.! H.onEvent CreateItemEH

        -- items
        unless (null items) $ do
            H.section H.! A.id "main" $ do
              checkbox (numTodo == 0)
                  H.! A.id "toggle-all"
                  H.! H.onEvent ToggleAllEH
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
                    H.! H.onEvent ClearCompletedEH
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


renderTodoItem :: EditFocus -> (Int, TodoItem) -> H.Html TodoEventHandler
renderTodoItem mbEditFocus (itemIdx, TodoItem done desc) = do
   H.li H.! itemClass
        H.! A.key (H.toValue itemIdx)
     $ do H.div H.! A.class_ "view" $ do
             checkbox done
                 H.! A.class_ "toggle"
                 H.! H.onEvent (ToggleItemEH itemIdx)
             H.label
                 H.! H.onEvent (EditItemEH itemIdx) $ H.toHtml desc
             H.button
                 H.! A.class_ "destroy"
                 H.! H.onEvent (DeleteItemEH itemIdx)
                 $ mempty
          case mbEditFocus of
           Just (focusIdx, focusText)
               | focusIdx == itemIdx ->
                   H.input H.! A.class_ "edit"
                           H.! A.value (H.toValue focusText)
                           H.! A.autofocus True
                           H.! H.onEvent EditInputEH
               | otherwise -> mempty
           Nothing         -> mempty
  where
    itemClass
      | isBeingEdited = A.class_ "editing"
      | done          = A.class_ "completed"
      | otherwise     = mempty

    isBeingEdited = Just itemIdx == fmap fst mbEditFocus

checkbox :: Bool -> H.Html ev
checkbox checked = H.input H.! A.type_ "checkbox" H.! A.checked checked



-- event handling
-----------------

handleTodoEvent :: UTCTime -> DOMEvent -> TodoEventHandler -> Maybe TodoAction
handleTodoEvent _t domEvent0 eventHandler = case eventHandler of
    CreateItemEH -> do
        do OnBlur <- domEvent
           return $ CreateItemA
        <|>
        do OnTextInputChange newText <- domEvent
           return $ UpdateNewItemDescA newText

    ToggleItemEH itemIdx -> do
        OnClick <- domEvent
        return $ TodoItemsActionA (ToggleItemA itemIdx)

    DeleteItemEH itemIdx -> do
        OnClick <- domEvent
        return $ TodoItemsActionA (DeleteItemA itemIdx)

    EditItemEH itemIdx -> do
        OnDoubleClick <- domEvent
        return $ EditItemA itemIdx

    EditInputEH ->
        do OnBlur <- domEvent
           return $ CommitAndStopEditingA
        <|>
        do OnTextInputChange newText <- domEvent
           return $ UpdateEditTextA newText

    ToggleAllEH -> do
        OnClick <- domEvent
        return $ TodoItemsActionA ToggleAllItemsA

    ClearCompletedEH -> do
        OnClick <- domEvent
        return $ TodoItemsActionA ClearCompletedA
  where
    domEvent = Just domEvent0


-- Testing the renderer
-----------------------

-- test :: IO ()
-- test = putStrLn $ H.Pretty.renderHtml $ render q0

{-
testCompact :: IO ()
testCompact =
    putStrLn $ H.String.renderHtml $ renderTodoState q0
  where
    items = [TodoItem True "DoNe", TodoItem False "Woaaaaah!"]

    q0 :: TodoState
    q0 = TodoState (Just (1, "OLD")) (items ++ items)
-}


------------------------------------------------------------------------------
-- Defining and running the app
------------------------------------------------------------------------------

todoApp :: App TodoState TodoAction TodoEventHandler
todoApp = App
    { appInitialState    = q0 {- TodoState
        { _tsEditFocus = Nothing
        , _tsItems     = []
        } -}
    , appInitialRequests = []
    , appApplyAction     = applyTodoAction
    , appRender          = renderTodoState
    , appHandleEvent     = handleTodoEvent
    }
  where
    items = [TodoItem True "DoNe", TodoItem False "Woaaaaah!"]

    q0 :: TodoState
    q0 = TodoState "" Nothing (items ++ items)

