{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A preliminary renderer that produces `ReactJS` components when run using
-- GHCJS.
--
module Text.Blaze.Renderer.ReactJS
    ( ReactJSNode
    , renderHtml
    ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either ( runEitherT, EitherT(..), left)

import qualified Data.ByteString.Char8 as SBC
import qualified Data.HashMap.Strict   as HMS
import           Data.List             (isInfixOf)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.ByteString       as S

import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Marshal         as Marshal
import           GHCJS.Types           (JSString, JSRef, JSArray, JSObject, castRef)

import           Prelude               hiding (span)

import           Text.Blaze.Internal
import           Text.Blaze.Event.Internal
import           Text.Blaze.Event.Charcode   (unCharcode)
import           Text.Blaze.Event.Keycode    (unKeycode)


------------------------------------------------------------------------------
-- FFI to ReactJS
------------------------------------------------------------------------------

data ReactJSEvent_
type ReactJSEvent = JSRef ReactJSEvent_

data ReactJSNode_
type ReactJSNode = JSRef ReactJSNode_

type ReactJSNodes = JSArray ReactJSNode


foreign import javascript unsafe
    "h$reactjs.mkDomNode($1, $2, $3)"
    mkReactJSParent
        :: JSString -> JSObject JSString -> ReactJSNodes -> IO ReactJSNode

foreign import javascript unsafe
    "h$reactjs.mkDomNode($1, $2, [])"
    mkReactJSLeaf :: JSString -> JSObject JSString -> IO ReactJSNode

foreign import javascript unsafe
    "$1.preventDefault()"
    preventDefault :: ReactJSEvent -> IO ()

foreign import javascript unsafe
    "$1.stopPropagation()"
    stopPropagation :: ReactJSEvent -> IO ()


------------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------------


-- TODO (SM): find a better representation for the rendering of Strings.
-- Probably a DList T.Text with a following concat.

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> String        -- ^ String to append
                 -> String        -- ^ Resulting string
fromChoiceString (Static s)     = getString s
fromChoiceString (String s)     = (s ++)
fromChoiceString (Text s)       = (T.unpack s ++)
fromChoiceString (ByteString s) = (SBC.unpack s ++)
fromChoiceString (PreEscaped x) =
    -- FiXME (SM): here we actually need to unescape!
    case x of
      String s -> (s ++)
      Text   s -> (\k -> T.foldr (:) k s)
      s        -> fromChoiceString s
fromChoiceString (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then id else (s ++)
    Text   s     -> if "</" `T.isInfixOf` s then id else (\k -> T.foldr (:) k s)
    ByteString s -> if "</" `S.isInfixOf` s then id else (SBC.unpack s ++)
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x . fromChoiceString y
fromChoiceString EmptyChoiceString = id


-- | Render some 'Markup' to a virtual dom.
--
-- This function is morally pure.
--
render
    :: forall act.
       Show act
    => (act -> Bool -> IO ())  -- ^ Callback for actions raised by event handlers.
    -> Markup act
    -> IO ReactJSNodes
render handleAct0 markup = do
    children <- Foreign.newArray
    go handleAct0 (\_props -> return ()) children markup
    return children
  where
    go :: forall act' b.
          (act' -> Bool -> IO ())
       -> (JSObject JSString -> IO ())
       -> (JSArray ReactJSNode)
       -> MarkupM act' b
       -> IO ()
    go handleAct setProps children html0 = case html0 of
        MapActions f h ->
            go (handleAct . f) setProps children h

        OnEvent handler h -> do
            let setProps' props = do
                    registerEventHandler (handleAct <$> handler) props
                    setProps props

            go handleAct setProps' children h

        Parent tag _open _close h -> tagToVNode (staticStringToJs tag) h
        CustomParent tag h        -> tagToVNode (choiceStringToJs tag) h
        Leaf tag _begin _end      -> leafToVNode (staticStringToJs tag)
        CustomLeaf tag _close     -> leafToVNode (choiceStringToJs tag)
        Content content           -> textToVNode (choiceStringToJs content)

        AddAttribute key _preparedKey value h -> do
            setProperty (staticStringToJs key) (choiceStringToJs value) h

        AddBoolAttribute key value h -> do
            setProperty (staticStringToJs key) (Foreign.toJSBool value) h

        -- FIXME (SM): This is not going to work in all cases, as 'attributes'
        -- must be set differently from properties.
        AddCustomAttribute key value h ->
            setProperty (choiceStringToJs key) (choiceStringToJs value) h

        AddObjectAttribute key object h -> do
            jsObj <- toJSRef_hashMap object
            setProperty (staticStringToJs key) jsObj h

        Empty           -> return ()
        Append h1 h2    -> do
            go handleAct setProps children h1
            go handleAct setProps children h2
      where
        choiceStringToJs cs = Foreign.toJSString (fromChoiceString cs "")
        staticStringToJs ss = Foreign.toJSString (getText ss)

        -- setProperty :: JSString -> JSRef a -> MarkupM (EventHandler act') b -> IO ()
        setProperty key value content =
            go handleAct setProps' children content
          where
            setProps' props =
                Foreign.setProp key value props >> setProps props

        makePropertiesObject = do
            props <- Foreign.newObj
            setProps props
            return props

        tagToVNode tag content = do
            props         <- makePropertiesObject
            innerChildren <- Foreign.newArray
            go handleAct (\_props -> return ()) innerChildren content
            node <- mkReactJSParent tag props innerChildren
            Foreign.pushArray node children

        leafToVNode tag = do
            props <- makePropertiesObject
            node  <- mkReactJSLeaf tag props
            Foreign.pushArray node children

        textToVNode :: JSString -> IO ()
        textToVNode jsText = Foreign.pushArray jsText children

-- TODO (asayers): Something like this should probably be added to GHCJS.Marshall:
-- toJSRef_hashMap :: (IsString a, ToJSRef b)
--                 => HMS.HashMap a b
--                 -> IO (JSRef (HMS.HashMap a b))
toJSRef_hashMap :: HMS.HashMap T.Text T.Text -> IO (JSRef (HMS.HashMap T.Text T.Text))
toJSRef_hashMap hashmap = fmap castRef $ do
    obj <- Foreign.newObj
    let addProp k v = Foreign.setProp k (Foreign.toJSString v) obj
    void $ HMS.traverseWithKey addProp hashmap
    return obj

renderHtml
    :: Show act
    => (act -> Bool -> IO ())
    -> Markup act
    -> IO (ReactJSNode)
renderHtml handleAction html = do
    children <- render handleAction html
    props <- Foreign.newObj
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

lookupProp :: JSString -> JSRef a -> EitherT T.Text IO (JSRef b)
lookupProp name obj = do
    mbProp <- lift $ Foreign.getPropMaybe name obj
    maybe (left err) return mbProp
  where
    err = "failed to get property '" <> Foreign.fromJSString name <> "'."

lookupIntProp :: JSString -> JSRef a -> EitherT T.Text IO Int
lookupIntProp name obj = do
    ref <- lookupProp name obj
    mbInt <- lift $ Marshal.fromJSRef ref
    case mbInt of
      Nothing -> left "lookupIntProp: couldn't parse field as Int"
      Just x  -> return x

lookupDoubleProp :: JSString -> JSRef a -> EitherT T.Text IO Double
lookupDoubleProp name obj = do
    ref <- lookupProp name obj
    mbDouble <- lift $ Marshal.fromJSRef ref
    case mbDouble of
      Nothing -> left "lookupDoubleProp: couldn't parse field as Double"
      Just x  -> return x

data Handler
    = IgnoreEvent
    | HandleEvent (IO (Bool -> IO ()))
      -- ^ Contains an IO action which generates the callback to attach to the event

registerEventHandler
    :: EventHandler (Bool -> IO ())
    -> JSObject JSString
       -- ^ Properties to register the event handler in
    -> IO ()
registerEventHandler eh props = case eh of
    OnKeyDown keys mkAct     -> register True OnKeyDownE      $ \eventRef ->
      handleKeyEvent eventRef keys mkAct
    OnKeyUp keys mkAct       -> register True OnKeyUpE        $ \eventRef ->
      handleKeyEvent eventRef keys mkAct
    OnKeyPress chars mkAct   -> register True OnKeyPressE     $ \eventRef ->
      handleCharEvent eventRef chars mkAct

    OnFocus mkAct            -> register False OnFocusE       $ \_eventRef ->
      return $ Right $ HandleEvent mkAct
    OnBlur mkAct             -> register False OnBlurE        $ \_eventRef ->
      return $ Right $ HandleEvent mkAct

    OnValueChange mkAct      -> register True  OnChangeE      $ \eventRef ->
      runEitherT $ do
        valueRef <- lookupProp "value" =<< lookupProp "target" eventRef
        return $ HandleEvent $ mkAct $ Foreign.fromJSString valueRef
    OnCheckedChange mkAct    -> register False OnChangeE      $ \eventRef ->
      runEitherT $ do
        valueRef <- lookupProp "checked" =<< lookupProp "target" eventRef
        return $ HandleEvent $ mkAct $ Foreign.fromJSBool valueRef
    OnSubmit mkAct           -> register True  OnSubmitE      $ \_eventRef ->
      return $ Right $ HandleEvent mkAct

    OnClick btns mkAct       -> register False OnClickE       $ \eventRef ->
      handleMouseEvent eventRef btns mkAct
    OnDoubleClick btns mkAct -> register False OnDoubleClickE $ \eventRef ->
      handleMouseEvent eventRef btns mkAct
    OnMouseDown btns mkAct   -> register False OnMouseDownE   $ \eventRef ->
      handleMouseEvent eventRef btns mkAct
    OnMouseUp btns mkAct     -> register False OnMouseUpE     $ \eventRef ->
      handleMouseEvent eventRef btns mkAct
    OnMouseMove mkAct        -> register False OnMouseMoveE   $ \eventRef ->
      runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    OnMouseEnter mkAct       -> register False OnMouseEnterE  $ \eventRef ->
      runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    OnMouseLeave mkAct       -> register False OnMouseLeaveE  $ \eventRef ->
      runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    OnMouseOver mkAct        -> register False OnMouseOverE   $ \eventRef ->
      runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    OnMouseOut mkAct         -> register False OnMouseOutE    $ \eventRef ->
      runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef

    OnScroll mkAct           -> register False OnScrollE      $ \eventRef ->
      runEitherT $ do
        scrollTop <- lookupIntProp "scrollTop" =<<lookupProp "target" eventRef
        return $ HandleEvent $ mkAct scrollTop

    OnWheel mkAct            -> register False OnWheelE       $ \eventRef ->
      runEitherT $ do
        dx <- lookupDoubleProp "deltaX" eventRef
        dy <- lookupDoubleProp "deltaY" eventRef
        dz <- lookupDoubleProp "deltaZ" eventRef
        let deltaValue = DeltaValue dx dy dz
        deltaMode <- lookupIntProp "deltaMode" eventRef
        domDelta <- case deltaMode of
              0 -> return $ PixelDelta deltaValue
              1 -> return $ LineDelta deltaValue
              2 -> return $ PageDelta deltaValue
              _ -> left "registerEventHandler: unrecognized delta mode"
        return $ HandleEvent $ mkAct domDelta




  where
    handleKeyEvent eventRef keys mkAct = runEitherT $ do
        keycode <- lookupIntProp "keyCode" eventRef <|>
                   lookupIntProp "which" eventRef
        if keycode `elem` map unKeycode keys
          then return $ HandleEvent mkAct
          else return $ IgnoreEvent

    handleCharEvent eventRef chars mkAct = runEitherT $ do
        charcode <- lookupIntProp "charCode" eventRef <|>
                    lookupIntProp "which" eventRef
        if charcode `elem` map unCharcode chars
          then return $ HandleEvent mkAct
          else return $ IgnoreEvent


    handleMouseEvent
        :: ReactJSEvent
        -> [MouseButton]
        -> (MousePosition -> IO (Bool -> IO ()))
        -> IO (Either T.Text Handler)
    handleMouseEvent eventRef btns mkAct = runEitherT $ do
        button <- getMouseButton eventRef
        if button `elem` btns
          then HandleEvent . mkAct <$> getMousePosition eventRef
          else return IgnoreEvent

    getMouseButton :: ReactJSEvent -> EitherT T.Text IO MouseButton
    getMouseButton eventRef = do
        button <- lookupIntProp "button" eventRef
        case button of
          0 -> return LeftButton
          1 -> return MiddleButton
          2 -> return RightButton
          _ -> left "getMouseButton: couldn't parse button code"

    getMousePosition :: ReactJSEvent -> EitherT T.Text IO MousePosition
    getMousePosition eventRef = do
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


    register
        :: Bool
        -> ReactJSEventType
        -> (ReactJSEvent -> IO (Either T.Text Handler))
           -- ^ Callback to actually handle the event.
        -> IO ()
    register requireSyncRedraw reactEvent extractHandler = do
        -- FIXME (SM): memory leak to to AlwaysRetain. Need to hook-up ReactJS
        -- event handler table with GHCJS GC.
        cb <- Foreign.syncCallback1 Foreign.AlwaysRetain False $ \eventRef -> do
            -- try to extract handler
            errOrHandler <- extractHandler eventRef

            case errOrHandler of
              Left err -> do
                  -- prevent default action and cancel propagation
                  preventDefault eventRef
                  stopPropagation eventRef
                  -- print the error
                  let eventName = Foreign.fromJSString $ reactEventName reactEvent
                  eventType <- either (const "Unknown type") Foreign.fromJSString <$>
                    runEitherT (lookupProp "type" eventRef)
                  putStrLn $ unlines
                    [ "blaze-react - event handling error: " ++ T.unpack err
                    , "Event was " ++ eventName ++ " of type " ++ eventType
                    ]
              Right IgnoreEvent -> return ()
              Right (HandleEvent mkHandler) -> do
                  -- prevent default action and cancel propagation
                  preventDefault eventRef
                  stopPropagation eventRef
                  -- run the handler. This triggers a redraw.
                  handler <- mkHandler
                  handler requireSyncRedraw

        Foreign.setProp (reactEventName reactEvent) cb props
