{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}

module Main (main) where

import           Prelude hiding (div)


import           Control.Applicative
import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception         (evaluate)
import           Control.Lens
                 ( makeLenses, view, preview, traverse, folded, set, over, ix
                 , to, _2, _Just, sumOf
                 )
import           Control.Monad

import           Data.Foldable   (foldMap)
import           Data.Monoid     ((<>), mempty)
import qualified Data.Text       as T
import           Data.Time       (UTCTime, getCurrentTime)

import           GHCJS.Types           (JSRef, JSString)
import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Foreign.QQ      (js, js_)
import qualified GHCJS.Prim            as Prim
import qualified GHCJS.VDOM            as VirtualDom

import           Safe                  (readMay)

import           System.IO             (fixIO)

import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import qualified Text.Blaze.Renderer.String           as Blaze.String
import qualified Text.Blaze.Renderer.VirtualDom       as Blaze.VirtualDom


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
    | OnTextInputBlur !T.Text
      -- ^ A text input-field lost focus and it's value was the given text.

data App state action eventHandler = App
    { appInitialState :: state
    , appApplyAction  :: action -> state -> state
    , appRender       :: state -> H.Html eventHandler
    , appHandleEvent  :: UTCTime -> DOMEvent -> eventHandler -> Maybe action
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
        unless (null items) $ do
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


-- event handling
-----------------

handleTodoEvent :: UTCTime -> DOMEvent -> TodoEventHandler -> Maybe TodoAction
handleTodoEvent _t domEvent0 eventHandler = case eventHandler of
    CreateItemEH -> do
        OnTextInputBlur newDesc <- domEvent
        return $ TodoItemsActionA (CreateItemA newDesc)

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
        do OnTextInputBlur _ <- domEvent
           return $ CommitAndStopEditingA
        <|>
        do OnTextInputChange newText <- domEvent
           return $ UpdateEditTextA newText

    MarkAllAsCompleteEH -> do
        OnClick <- domEvent
        return $ TodoItemsActionA MarkAllAsCompleteA
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
    { appInitialState = q0 {- TodoState
        { _tsEditFocus = Nothing
        , _tsItems     = []
        } -}
    , appApplyAction = applyTodoAction
    , appRender      = renderTodoState
    , appHandleEvent = handleTodoEvent
    }
  where
    items = [TodoItem True "DoNe", TodoItem False "Woaaaaah!"]

    q0 :: TodoState
    q0 = TodoState (Just (1, "OLD")) (items ++ items)


-- generic runApp function
--------------------------


atAnimationFrame :: IO () -> IO ()
atAnimationFrame io = do
    cb <- fixIO $ \cb ->
        Foreign.syncCallback Foreign.AlwaysRetain
                             False
                             (Foreign.release cb >> io)
    [js_| window.requestAnimationFrame(`cb); |]

-- lookupEventHandlerName :: JSRef () -> IO (Maybe String)
-- lookupEventHandlerName eventRef = do
--     nullOrJSString <- [js| h$vdom.lookupBlazeEventHandlerName(`eventRef) |]
--     Marshall.fromJSRef nullOrJSString
--
lookupEventHandlerName :: JSRef () -> IO (Maybe String)
lookupEventHandlerName eventRef = do
    mbNameRef <- js_lookupEventHandlerName eventRef
    return $
        if Prim.isNull mbNameRef
          then Nothing
          else Just (Prim.fromJSString mbNameRef)

foreign import javascript unsafe
  "$1.target.getAttribute(\"data-on-blaze-event\")"
  js_lookupEventHandlerName :: JSRef () -> IO JSString

runApp :: (Show eh, Read eh, Show act) => App st act eh -> IO ()
runApp (App initialState apply renderAppState handleEvent) = do
    -- create root element in body for the app
    root <- [js| document.createElement('div') |]
    [js_| document.body.appendChild(`root); |]

    -- create virtual DOM node corresponding to the empty root div
    rootVNode <- VirtualDom.vnode "div"
                   <$> VirtualDom.newProperties
                   <*> VirtualDom.newChildren

    -- create global state variable
    stateVar <- newMVar (False, initialState, rootVNode)


    let redraw :: IO ()
        redraw = modifyMVar_ stateVar $ \(_requestedRedraw, state, oldVNode) -> do
            newVNode <- Blaze.VirtualDom.renderHtml (renderAppState state)
            patch <- evaluate (VirtualDom.diff oldVNode newVNode)
            VirtualDom.applyPatch root patch
            return (False, state, newVNode)


        makeOnClickCallback =
          Foreign.syncCallback1 Foreign.AlwaysRetain False $ \event -> do

              mbEventHandlerName <- lookupEventHandlerName event

              case mbEventHandlerName of
                Nothing -> putStrLn "No event handler found."
                Just eventHandlerName -> case readMay eventHandlerName of
                  Nothing -> putStrLn $
                      "Could not parse event handler name: " ++ eventHandlerName
                  Just eventHandler -> do
                    t <- getCurrentTime
                    case handleEvent t OnClick eventHandler of
                      Nothing -> putStrLn $
                        "Event handler '" ++ show eventHandler ++ "' rejected on-click event."
                      Just action -> do
                        putStrLn $ "Event handler '" ++ show eventHandler ++
                                   "' generated action: " ++ show action
                        modifyMVar_ stateVar $ \(requestedRedraw, state, oldVNode) -> do
                            unless requestedRedraw $ atAnimationFrame redraw
                            return (True, apply action state, oldVNode)

    -- install click event handler on the root
    cb <- makeOnClickCallback

    [js_| `root.addEventListener("click", `cb, false)|]

    -- request a redraw for the initial state
    let initialHtml = renderAppState initialState
    putStrLn "Starting app with initial state rendered as:"
    putStrLn $ Blaze.String.renderHtml initialHtml

    atAnimationFrame redraw

    putStrLn "Started app -- TODO: install event handlers"
    forever $ threadDelay 10000000



-- our main function
--------------------

main :: IO ()
main = runApp todoApp

