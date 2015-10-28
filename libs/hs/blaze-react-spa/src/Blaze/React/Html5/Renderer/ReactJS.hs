{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A preliminary renderer that produces `ReactJS` components when run using
-- GHCJS.
--
module Blaze.React.Html5.Renderer.ReactJS
    ( ReactJSNode
    , renderHtml
    ) where

import           Blaze.React.Html5.Event.Internal
-- import           Blaze.React.Html5.Event.Charcode   (unCharcode)
import           Blaze.React.Html5.Event.Keycode    (Keycode(..))
import           Blaze.React.Markup.Internal

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either ( runEitherT, EitherT(..), left)

-- import qualified Data.ByteString.Char8 as SBC
import qualified Data.HashMap.Strict   as HMS
-- import           Data.List             (isInfixOf)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
-- import qualified Data.ByteString       as S

import qualified GHCJS.Foreign.Internal as Foreign
import qualified GHCJS.Foreign.Callback as Foreign
import           GHCJS.Marshal          as Marshal
import           GHCJS.Marshal.Pure     as Marshal
import           GHCJS.Types            (JSVal, jsval)

import           JavaScript.Array      (MutableJSArray)
import qualified JavaScript.Array      as Array

import           JavaScript.Object.Internal (Object(..))
import qualified JavaScript.Object          as Object

import           Data.JSString         (JSString)
import qualified Data.JSString         as JSString
import qualified Data.JSString.Text    as JSString

import           Prelude               hiding (span)

------------------------------------------------------------------------------
-- FFI to ReactJS
------------------------------------------------------------------------------

type ReactJSEvent = JSVal

type ReactJSNode = JSVal

type ReactJSNodes = MutableJSArray

foreign import javascript unsafe
    "h$reactjs.mkDomNode($1, $2, $3)"
    mkReactJSParent
        :: JSString -> Object -> ReactJSNodes -> IO ReactJSNode

foreign import javascript unsafe
    "h$reactjs.mkDomNode($1, $2, [])"
    mkReactJSLeaf :: JSString -> Object -> IO ReactJSNode

foreign import javascript unsafe
    "$1.preventDefault()"
    preventDefault :: ReactJSEvent -> IO ()

foreign import javascript unsafe
    "$1.stopPropagation()"
    stopPropagation :: ReactJSEvent -> IO ()


------------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------------

-- | Render a 'ChoiceString'.
--
choiceStringToJs :: ChoiceString -> JSString
choiceStringToJs cs = case cs of
  Static s -> getJSString s
  String s -> JSString.pack s
  Text s -> JSString.textToJSString s
  JSString s -> s
  AppendChoiceString x y -> appendChoiceString x y
  EmptyChoiceString -> ""

type DList a = [a] -> [a]

appendChoiceString :: ChoiceString -> ChoiceString -> JSString
appendChoiceString x y = JSString.concat ((go x . go y) [])
  where
    go :: ChoiceString -> DList JSString
    go (AppendChoiceString a b) = go a . go b
    go EmptyChoiceString = id
    go s = (choiceStringToJs s :)

-- | Render some 'Markup' to a virtual dom.
--
-- This function is morally pure.
--
render
    :: forall act.
       (act -> Bool -> IO ())
       -- ^ Callback for actions raised by event handlers.
    -> Markup (EventHandler act)
    -> IO ReactJSNodes
render handleAct0 markup = do
    children <- Array.create
    go handleAct0 (\_props -> return ()) children markup
    return children
  where
    go :: forall act'.
          (act' -> Bool -> IO ())
       -> (Object -> IO ())
       -> MutableJSArray
       -> Markup (EventHandler act')
       -> IO ()
    go handleAct setProps children html0 = case html0 of
        -- MapActions f h ->
        --     go (handleAct . f) setProps children h

        OnEvent handler h -> do
            let setProps' props = do
                    registerEventHandler (handleAct <$> handler) props
                    setProps props

            go handleAct setProps' children h

        Parent tag h -> tagToVNode (choiceStringToJs tag) h
        -- CustomParent tag h        -> tagToVNode (choiceStringToJs tag) h
        Leaf tag _close    -> leafToVNode (choiceStringToJs tag)
        -- CustomLeaf tag _close     -> leafToVNode (choiceStringToJs tag)
        Content content           -> textToVNode (choiceStringToJs content)
        AddAttribute key value h -> do
            setProperty (choiceStringToJs key) (jsval (choiceStringToJs value)) h

        AddBoolAttribute key value h -> do
            setProperty (choiceStringToJs key) (Foreign.toJSBool value) h

        -- FIXME (SM): This is not going to work in all cases, as 'attributes'
        -- must be set differently from properties.
        AddCustomAttribute key value h ->
            setProperty (choiceStringToJs key) (jsval (choiceStringToJs value)) h

        AddObjectAttribute key object h -> do
            jsObj <- toJSRef_hashMap object
            setProperty (choiceStringToJs key) (jsval jsObj) h

        Empty           -> return ()
        Append h1 h2    -> do
            go handleAct setProps children h1
            go handleAct setProps children h2
      where
        -- setProperty :: JSString -> JSRef a -> MarkupM (EventHandler act') b -> IO ()
        setProperty key value content =
            go handleAct setProps' children content
          where
            setProps' props =
                Object.setProp key value props >> setProps props

        makePropertiesObject = do
            props <- Object.create
            setProps props
            return props

        tagToVNode tag content = do
            props         <- makePropertiesObject
            innerChildren <- Array.create
            go handleAct (\_props -> return ()) innerChildren content
            node <- mkReactJSParent tag props innerChildren
            Array.push node children

        leafToVNode tag = do
            props <- makePropertiesObject
            node  <- mkReactJSLeaf tag props
            Array.push node children

        textToVNode :: JSString -> IO ()
        textToVNode jsText = Array.push (jsval jsText) children

-- TODO (asayers): Something like this should probably be added to GHCJS.Marshall:
-- toJSRef_hashMap :: (IsString a, ToJSRef b)
--                 => HMS.HashMap a b
--                 -> IO (JSRef (HMS.HashMap a b))
toJSRef_hashMap :: HMS.HashMap T.Text T.Text -> IO Object
toJSRef_hashMap hashmap = do
    obj <- Object.create
    let addProp k v = do
          let k' = JSString.textToJSString k
          let v' = jsval (JSString.textToJSString v)
          Object.setProp k' v' obj
    void $ HMS.traverseWithKey addProp hashmap
    return obj

renderHtml
    :: (act -> Bool -> IO ())
    -> Markup (EventHandler act)
    -> IO (ReactJSNode)
renderHtml handleAction html = do
    children <- render handleAction html
    props <- Object.create
    mkReactJSParent "div" props children


------------------------------------------------------------------------------
-- Event handler callback construction
------------------------------------------------------------------------------

-- | ReactJS defines the following event types, as of v0.12:
data ReactJSEventType
      -- Clipboard Events
    = OnCopyE | OnCutE | OnPasteE
      -- Keyboard Events
    | OnKeyDownE | OnKeyPressE | OnKeyUpE
      -- Focus Events
    | OnFocusE | OnBlurE
      -- Form Events
    | OnChangeE | OnInputE | OnSubmitE
      -- Mouse Events
    | OnClickE | OnDoubleClickE | OnDragE | OnDragEndE | OnDragEnterE
    | OnDragExitE | OnDragLeaveE | OnDragOverE | OnDragStartE | OnDropE
    | OnMouseDownE | OnMouseEnterE | OnMouseLeaveE | OnMouseMoveE
    | OnMouseOutE | OnMouseOverE | OnMouseUpE
      -- Touch Events
    | OnTouchCancelE | OnTouchEndE | OnTouchMoveE | OnTouchStartE
      -- UI Events
    | OnScrollE
      -- Wheel Events
    | OnWheelE

reactEventName :: ReactJSEventType -> JSString
reactEventName ev = case ev of
    OnCopyE        -> "onCopy"
    OnCutE         -> "onCut"
    OnPasteE       -> "onPaste"
    OnKeyDownE     -> "onKeyDown"
    OnKeyPressE    -> "onKeyPress"
    OnKeyUpE       -> "onKeyUp"
    OnFocusE       -> "onFocus"
    OnBlurE        -> "onBlur"
    OnChangeE      -> "onChange"
    OnInputE       -> "onInput"
    OnSubmitE      -> "onSubmit"
    OnClickE       -> "onClick"
    OnDoubleClickE -> "onDoubleClick"
    OnDragE        -> "onDrag"
    OnDragEndE     -> "onDragEnd"
    OnDragEnterE   -> "onDragEnter"
    OnDragExitE    -> "onDragExit"
    OnDragLeaveE   -> "onDragLeave"
    OnDragOverE    -> "onDragOver"
    OnDragStartE   -> "onDragStart"
    OnDropE        -> "onDrop"
    OnMouseDownE   -> "onMouseDown"
    OnMouseEnterE  -> "onMouseEnter"
    OnMouseLeaveE  -> "onMouseLeave"
    OnMouseMoveE   -> "onMouseMove"
    OnMouseOutE    -> "onMouseOut"
    OnMouseOverE   -> "onMouseOver"
    OnMouseUpE     -> "onMouseUp"
    OnTouchCancelE -> "onTouchCancel"
    OnTouchEndE    -> "onTouchEnd"
    OnTouchMoveE   -> "onTouchMove"
    OnTouchStartE  -> "onTouchStart"
    OnScrollE      -> "onScroll"
    OnWheelE       -> "onWheel"

getPropMaybe :: JSString -> JSVal -> IO (Maybe JSVal)
getPropMaybe p o = do
  p' <- Object.getProp p (Object o)
  if Foreign.isUndefined p' then return Nothing else return (Just p')
{-# INLINE getPropMaybe #-}

lookupProp :: JSString -> JSVal -> EitherT T.Text IO JSVal
lookupProp name obj = do
    mbProp <- lift $ getPropMaybe name obj
    maybe (left err) return mbProp
  where
    err = "failed to get property '" <> JSString.textFromJSString name <> "'."

lookupIntProp :: JSString -> JSVal -> EitherT T.Text IO Int
lookupIntProp name obj = do
    val <- lookupProp name obj
    mbInt <- lift $ Marshal.fromJSVal val
    case mbInt of
      Nothing -> left "lookupIntProp: couldn't parse field as Int"
      Just x  -> return x

{-
lookupDoubleProp :: JSString -> JSVal  -> EitherT T.Text IO Double
lookupDoubleProp name obj = do
    ref <- lookupProp name obj
    mbDouble <- lift $ Marshal.fromJSRef ref
    case mbDouble of
      Nothing -> left "lookupDoubleProp: couldn't parse field as Double"
      Just x  -> return x
-}

data Handler
    = IgnoreEvent
    | HandleEvent (Bool -> IO ())
      -- ^ Contains an IO action which generates the callback to attach to the event

registerEventHandler
    :: EventHandler (Bool -> IO ())
    -> Object
       -- ^ Properties to register the event handler in
    -> IO ()
registerEventHandler (EventHandler sel mkAct) props = case sel of
    OnKeyDown keys           -> register True OnKeyDownE      $ \eventRef ->
      handleKeyEvent eventRef keys mkAct
    -- OnKeyUp keys mkAct       -> register True OnKeyUpE        $ \eventRef ->
    --   handleKeyEvent eventRef keys mkAct
    -- OnKeyPress chars mkAct   -> register True OnKeyPressE     $ \eventRef ->
    --   handleCharEvent eventRef chars mkAct

    -- OnFocus mkAct            -> register False OnFocusE       $ \_eventRef ->
    --   return $ Right $ HandleEvent mkAct
    -- OnBlur mkAct             -> register False OnBlurE        $ \_eventRef ->
    --   return $ Right $ HandleEvent mkAct

    OnValueChange            -> register True  OnChangeE      $ \eventRef ->
      runEitherT $ do
        value <- lookupProp "value" =<< lookupProp "target" eventRef
        return . HandleEvent . mkAct . JSString.textFromJSString . Marshal.pFromJSVal $ value
    -- OnCheckedChange mkAct    -> register False OnChangeE      $ \eventRef ->
    --   runEitherT $ do
    --     valueRef <- lookupProp "checked" =<< lookupProp "target" eventRef
    --     return $ HandleEvent $ mkAct $ Foreign.fromJSBool valueRef
    -- OnSubmit mkAct           -> register True  OnSubmitE      $ \_eventRef ->
    --   return $ Right $ HandleEvent mkAct

    OnClick btns             -> register False OnClickE       $ \eventRef ->
      handleMouseEvent eventRef btns mkAct
    -- OnDoubleClick btns mkAct -> register False OnDoubleClickE $ \eventRef ->
    --   handleMouseEvent eventRef btns mkAct
    -- OnMouseDown btns mkAct   -> register False OnMouseDownE   $ \eventRef ->
    --   handleMouseEvent eventRef btns mkAct
    -- OnMouseUp btns mkAct     -> register False OnMouseUpE     $ \eventRef ->
    --   handleMouseEvent eventRef btns mkAct
    -- OnMouseMove mkAct        -> register False OnMouseMoveE   $ \eventRef ->
    --   runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    -- OnMouseEnter mkAct       -> register False OnMouseEnterE  $ \eventRef ->
    --   runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    -- OnMouseLeave mkAct       -> register False OnMouseLeaveE  $ \eventRef ->
    --   runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    -- OnMouseOver mkAct        -> register False OnMouseOverE   $ \eventRef ->
    --   runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    -- OnMouseOut mkAct         -> register False OnMouseOutE    $ \eventRef ->
    --   runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef

    -- OnScroll mkAct           -> register False OnScrollE      $ \eventRef ->
    --   runEitherT $ do
    --     scrollTop <- lookupIntProp "scrollTop" =<<lookupProp "target" eventRef
    --     return $ HandleEvent $ mkAct scrollTop

    -- OnWheel mkAct            -> register False OnWheelE       $ \eventRef ->
    --   runEitherT $ do
    --     dx <- lookupDoubleProp "deltaX" eventRef
    --     dy <- lookupDoubleProp "deltaY" eventRef
    --     dz <- lookupDoubleProp "deltaZ" eventRef
    --     let deltaValue = DeltaValue dx dy dz
    --     deltaMode <- lookupIntProp "deltaMode" eventRef
    --     domDelta <- case deltaMode of
    --           0 -> return $ PixelDelta deltaValue
    --           1 -> return $ LineDelta deltaValue
    --           2 -> return $ PageDelta deltaValue
    --           _ -> left "registerEventHandler: unrecognized delta mode"
    --     return $ HandleEvent $ mkAct domDelta




  where
    register
        :: Bool
        -> ReactJSEventType
        -> (ReactJSEvent -> IO (Either T.Text Handler))
           -- ^ Callback to actually handle the event.
        -> IO ()
    register requireSyncRedraw reactEvent extractHandler = do
        -- FIXME (SM): memory leak to to AlwaysRetain. Need to hook-up ReactJS
        -- event handler table with GHCJS GC.
        cb <- Foreign.syncCallback1 Foreign.ThrowWouldBlock $ \eventRef -> do
            -- try to extract handler
            errOrHandler <- extractHandler eventRef

            case errOrHandler of
              Left err -> do
                  -- prevent default action and cancel propagation
                  preventDefault eventRef
                  stopPropagation eventRef
                  -- print the error
                  let eventName = JSString.unpack $ reactEventName reactEvent
                  eventType <- either (const "Unknown type") (JSString.unpack . pFromJSVal) <$>
                    runEitherT (lookupProp "type" eventRef)

                  putStrLn $ unlines
                    [ "blaze-react - event handling error: " ++ T.unpack err
                    , "Event was " ++ eventName ++ " of type " ++ eventType
                    ]
              Right IgnoreEvent -> return ()
              Right (HandleEvent handler) -> do
                  -- prevent default action and cancel propagation
                  preventDefault eventRef
                  stopPropagation eventRef
                  -- run the handler. This triggers a redraw.
                  handler requireSyncRedraw

        Object.setProp (reactEventName reactEvent) (jsval cb) props


handleKeyEvent
    :: ReactJSEvent
    -> [Keycode]
    -> (Keycode -> Bool -> IO ())
    -> IO (Either T.Text Handler)
handleKeyEvent eventRef keys mkAct = runEitherT $ do
    keycode <- lookupIntProp "keyCode" eventRef <|>
               lookupIntProp "which" eventRef
    if keycode `elem` map unKeycode keys
      then return $ HandleEvent (mkAct (Keycode keycode))
      else return $ IgnoreEvent

{-
handleCharEvent eventRef chars mkAct = runEitherT $ do
    charcode <- lookupIntProp "charCode" eventRef <|>
                lookupIntProp "which" eventRef
    if charcode `elem` map unCharcode chars
      then return $ HandleEvent mkAct
      else return $ IgnoreEvent
-}

handleMouseEvent
    :: ReactJSEvent
    -> [MouseButton]
    -> (MousePosition -> Bool -> IO ())
    -> IO (Either T.Text Handler)
handleMouseEvent eventRef btns mkAct = runEitherT $ do
    button <- getMouseButton
    if button `elem` btns
      then HandleEvent . mkAct <$> getMousePosition
      else return IgnoreEvent
  where
    getMouseButton :: EitherT T.Text IO MouseButton
    getMouseButton = do
        button <- lookupIntProp "button" eventRef
        case button of
          0 -> return LeftButton
          1 -> return MiddleButton
          2 -> return RightButton
          _ -> left "getMouseButton: couldn't parse button code"

    getMousePosition :: EitherT T.Text IO MousePosition
    getMousePosition = do
        clientX <- lookupIntProp "clientX" eventRef
        clientY <- lookupIntProp "clientY" eventRef
        pageX   <- lookupIntProp "pageX"   eventRef
        pageY   <- lookupIntProp "pageY"   eventRef
        screenX <- lookupIntProp "screenX" eventRef
        screenY <- lookupIntProp "screenY" eventRef
        return MousePosition
          { mpClientX = clientX
          , mpClientY = clientY
          , mpPageX   = pageX
          , mpPageY   = pageY
          , mpScreenX = screenX
          , mpScreenY = screenY
          }
