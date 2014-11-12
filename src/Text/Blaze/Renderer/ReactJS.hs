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
import           Data.List             (isInfixOf)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.ByteString       as S

import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Marshal         as Marshal
import           GHCJS.Types           (JSString, JSRef, JSArray, JSObject)

import           Prelude               hiding (span)

import           Text.Blaze.Internal
import           Text.Blaze.Keycode    (unKeycode)


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

lookupProp :: JSString -> JSRef a -> EitherT T.Text IO (JSRef b)
lookupProp name obj = do
    mbProp <- lift $ Foreign.getPropMaybe name obj
    maybe (left err) return mbProp
  where
    err = "failed to get property '" <> Foreign.fromJSString name <> "'."

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
    OnClick mkAct           -> register False "onClick"       "click"     $ simply mkAct
    OnDoubleClick mkAct     -> register False "onDoubleClick" "dblclick"  $ simply mkAct
    OnBlur mkAct            -> register False "onBlur"        "blur"      $ simply mkAct
    OnMouseOver mkAct       -> register False "onMouseOver"   "mouseover" $ simply mkAct
    OnTextInputChange mkAct -> register True  "onChange"      "input"     $ \eventRef ->
        runEitherT $ do
          targetRef <- lookupProp "target" eventRef
          valueRef  <- lookupProp "value" targetRef
          return $ HandleEvent $ mkAct $ Foreign.fromJSString valueRef
    OnKeyPress targetKeycode mkAct -> register True "onKeyPress" "keypress" $ \eventRef ->
        runEitherT $ do
          keycodeStr <- lookupProp "which" eventRef
          mbKeycode <- lift $ Marshal.fromJSRef keycodeStr
          case mbKeycode of
            Nothing -> left "Couldn't decode keycode"
            Just keycode
              | keycode == unKeycode targetKeycode -> return $ HandleEvent mkAct
              | otherwise                          -> return $ IgnoreEvent

  where
    simply = const . return . Right . HandleEvent

    register
        :: Bool
        -> JSString  -- ^ Property name under which to register the callback.
        -> T.Text    -- ^ Expected event type
        -> (ReactJSEvent -> IO (Either T.Text Handler))
           -- ^ Callback to actually handle the event.
        -> IO ()
    register requireSyncRedraw reactJsProp expectedType extractHandler = do
        -- FIXME (SM): memory leak to to AlwaysRetain. Need to hook-up ReactJS
        -- event handler table with GHCJS GC.
        cb <- Foreign.syncCallback1 Foreign.AlwaysRetain False $ \eventRef -> do
            -- try to extract handler
            errOrHandler <- runEitherT $ do
                eventType <- Foreign.fromJSString <$> lookupProp "type" eventRef
                if eventType == expectedType
                  then EitherT $ extractHandler eventRef
                  else left $ "event type '" <> eventType <>
                              "' /= expected type '" <> expectedType <> "'."

            case errOrHandler of
              Left err -> do
                  -- prevent default action and cancel propagation
                  preventDefault eventRef
                  stopPropagation eventRef
                  -- print the error
                  putStrLn $ "blaze-react - event handling error: " ++ T.unpack err
              Right IgnoreEvent -> return ()
              Right (HandleEvent mkHandler) -> do
                  -- prevent default action and cancel propagation
                  preventDefault eventRef
                  stopPropagation eventRef
                  -- run the handler. This triggers a redraw.
                  handler <- mkHandler
                  handler requireSyncRedraw

        Foreign.setProp reactJsProp cb props
