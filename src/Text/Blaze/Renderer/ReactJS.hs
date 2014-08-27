{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A preliminary renderer that produces `ReactJS` components when run using
-- GHCJS.
--
module Text.Blaze.Renderer.ReactJS
    ( renderHtml
    ) where

import Data.List (isInfixOf)

import qualified Data.ByteString.Char8 as SBC
import qualified Data.Text             as T
import qualified Data.ByteString       as S
import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Types           (JSString)

import           Prelude               hiding (span)

import           Text.Blaze.Internal


------------------------------------------------------------------------------
-- FFI to ReactJS
------------------------------------------------------------------------------

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
    :: (ev -> JSString)          -- ^ Serialization of the event handlers.
    -> Markup ev                 -- ^ Markup to render
    -> IO (VirtualDom.Children)  -- ^ Resulting Virtual DOM.
render showEv0 markup = do
    children <- VirtualDom.newChildren
    go showEv0 (\_props -> return ()) children markup
    return children
  where
    go :: forall ev b.
          (ev -> JSString)
       -> (JSObject () -> IO ())
       -> (JSArray ReactJSNode)
       -> MarkupM ev b
       -> IO ()
    go showEv setProps children html0 = case html0 of
        MapEvents _f h ->
            -- go (showEv . f) setProps children content
            go showEv setProps children h

        OnEvent ev h ->
            -- let setProps' props = do
            --        VirtualDom.setAttributes "data-on-blaze-event" (showEv ev) props
            --        setProps props
            -- in go showEv setProps' children content
            -- go showEv setProps children content
            -- setProperty "data-on-blaze-event" (showEv ev) h
            go showEv setProps h

        Parent tag _open _close h -> tagToVNode (staticStringToJs tag) h
        CustomParent tag h        -> tagToVNode (choiceStringToJs tag) h
        Leaf tag _begin _end      -> leafToVNode (staticStringToJs tag)
        CustomLeaf tag _close     -> leafToVNode (choiceStringToJs tag)
        Content content           -> textToVNode (choiceStringToJs content)

        AddAttribute key _preparedKey value h -> do
            setProperty (staticStringToJs key) (choiceStringToJs value) h

        -- FIXME (SM): This is not going to work in all cases, as 'attributes'
        -- must be set differently from properties.
        AddCustomAttribute key value h ->
            setProperty (choiceStringToJs key) (choiceStringToJs value) h

        Empty           -> return ()
        Append h1 h2    -> do
            go showEv setProps children h1
            go showEv setProps children h2
      where
        choiceStringToJs cs = Foreign.toJSString (fromChoiceString cs "")
        staticStringToJs ss = Foreign.toJSString (getText ss)

        setProperty :: JSString -> JSString -> MarkupM ev b -> IO ()
        setProperty key value content =
            go showEv setProps' children content
          where
            setProps' props =
                Foreign.setProp key value props >> setProps props

        makePropertiesObject = do
            props <- Foreign.newProps
            setProps props
            return props

        tagToVNode tag content = do
            props         <- makePropertiesObject
            innerChildren <- VirtualDom.newChildren
            go showEv (\_props -> return ()) innerChildren content
            node <- mkReactJSNode tag props innerChildren
            VirtualDom.pushChild vnode children

        leafToVNode tag = do
            props <- makePropertiesObject
            node  <- mkReactJSLeaf tag
            Foreign.pushArray node children

        textToVNode :: JSString -> IO ()
        textToVNode jsText = Foreign.pushArray jsText children


renderHtml :: Show ev => Markup ev -> IO (ReactJSNode)
renderHtml html = do
    children <- render (Foreign.toJSString . show) html
    props <- Foreign.newProps
    return $! mkReactJSParent "div" props children
