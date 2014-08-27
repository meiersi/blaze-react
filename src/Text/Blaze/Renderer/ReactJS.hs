{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A preliminary renderer that produces `ReactJS` components when run using
-- GHCJS.
--
module Text.Blaze.Renderer.ReactJS
    ( ReactJSNode
    , ReactJSEvent
    , EventType(..)
    , renderHtml
    ) where


import           Control.Monad         (forM_)

import qualified Data.ByteString.Char8 as SBC
import           Data.List             (isInfixOf)
import qualified Data.Text             as T
import qualified Data.ByteString       as S

import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Types           (JSString, JSRef, JSArray, JSObject, JSFun)

import           Prelude               hiding (span)

import           Text.Blaze.Internal


------------------------------------------------------------------------------
-- FFI to ReactJS
------------------------------------------------------------------------------

data ReactJSEvent_
type ReactJSEvent = JSRef ReactJSEvent_

data ReactJSNode_
type ReactJSNode = JSRef ReactJSNode_

type ReactJSNodes = JSArray ReactJSNode

data EventType
    = Click
    | DoubleClick

foreign import javascript unsafe
    "h$reactjs.mkDomNode($1, $2, $3)"
    mkReactJSParent
        :: JSString -> JSObject JSString -> ReactJSNodes -> IO ReactJSNode

foreign import javascript unsafe
    "h$reactjs.mkDomNode($1, $2, [])"
    mkReactJSLeaf :: JSString -> JSObject JSString -> IO ReactJSNode


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
    :: JSFun (ReactJSEvent -> IO ())
       -- ^ The one event handler callback to use when registering events.
    -> (ev -> (JSString, [EventType]))
       -- ^ Serialization of the event markers together with the list of
       -- events that they should register for.
    -> Markup ev        -- ^ Markup to render
    -> IO ReactJSNodes  -- ^ Resulting Virtual DOM.
render eventHandlerCb processEv0 markup = do
    children <- Foreign.newArray
    go processEv0 (\_props -> return ()) children markup
    return children
  where
    go :: forall ev b.
          (ev -> (JSString, [EventType]))
       -> (JSObject JSString -> IO ())
       -> (JSArray ReactJSNode)
       -> MarkupM ev b
       -> IO ()
    go processEv setProps children html0 = case html0 of
        MapEvents f h ->
            go (processEv . f) setProps children h

        OnEvent ev h -> case processEv ev of
          (_,      []        ) -> go processEv setProps children h
          (marker, eventTypes) -> do
            let setProps' props = do
                    Foreign.setProp ("data-blaze-id" :: JSString) marker props
                    forM_ eventTypes $ \eventType -> do
                        let event = case eventType of
                              Click       -> "onClick" :: JSString
                              DoubleClick -> "onDoubleClick"

                        Foreign.setProp event eventHandlerCb props
                    setProps props

            go processEv setProps' children h

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
            go processEv setProps children h1
            go processEv setProps children h2
      where
        choiceStringToJs cs = Foreign.toJSString (fromChoiceString cs "")
        staticStringToJs ss = Foreign.toJSString (getText ss)

        setProperty :: JSString -> JSRef a -> MarkupM ev b -> IO ()
        setProperty key value content =
            go processEv setProps' children content
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
            go processEv (\_props -> return ()) innerChildren content
            node <- mkReactJSParent tag props innerChildren
            Foreign.pushArray node children

        leafToVNode tag = do
            props <- makePropertiesObject
            node  <- mkReactJSLeaf tag props
            Foreign.pushArray node children

        textToVNode :: JSString -> IO ()
        textToVNode jsText = Foreign.pushArray jsText children


renderHtml
    :: Show ev
    => JSFun (ReactJSEvent -> IO ())
    -> (ev -> [EventType])
    -> Markup ev
    -> IO (ReactJSNode)
renderHtml eventHandlerCb toEventTypes html = do
    children <- render eventHandlerCb processEv html
    props <- Foreign.newObj
    mkReactJSParent "div" props children
  where
    processEv ev = (Foreign.toJSString (show ev), toEventTypes ev)
