

-- | A preliminary renderer that produces `virtual-dom` nodes when run using
-- GHCJS.
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Renderer.VirtualDom
    ( -- fromChoiceString
    -- , renderMarkup
      renderHtml
    ) where

import Data.List (isInfixOf)

import qualified Data.ByteString.Char8 as SBC
import qualified Data.Text             as T
import qualified Data.ByteString       as S
import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Types           (JSString)
import qualified GHCJS.VDOM            as VirtualDom

import Text.Blaze.Internal

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
renderAsVNodes
    :: (ev -> JSString)          -- ^ Serialization of the event handlers.
    -> Markup ev                 -- ^ Markup to render
    -> IO (VirtualDom.Children)  -- ^ Resulting Virtual DOM.
renderAsVNodes showEv0 markup = do
    children <- VirtualDom.newChildren
    go showEv0 (\_props -> return ()) children markup
    return children
  where
    go :: (ev -> JSString)
       -> (VirtualDom.Properties -> IO ())
       -> VirtualDom.Children
       -> MarkupM ev b
       -> IO ()
    go showEv setProps children html = case html of
        MapEvents f content ->
            go (showEv . f) setProps children content

        OnEvent ev content ->
            setAttribute "data-on-blaze-event" (showEv ev) content

        Parent tag _open _close h -> tagToVNode (staticStringToJs tag) h
        CustomParent tag h        -> tagToVNode (choiceStringToJs tag) h
        Leaf tag _begin _end      -> leafToVNode (staticStringToJs tag)
        CustomLeaf tag _close     -> leafToVNode (choiceStringToJs tag)
        Content content           -> textToVNode (choiceStringToJs content)

        AddAttribute _ key value h -> do
            setAttribute (staticStringToJs key) (choiceStringToJs value) h

        AddCustomAttribute key value h ->
            setAttribute (choiceStringToJs key) (choiceStringToJs value) h

        Empty           -> return ()
        Append h1 h2    -> do
            go showEv setProps children h1
            go showEv setProps children h2
      where
        choiceStringToJs cs = Foreign.toJSString (fromChoiceString cs "")
        staticStringToJs ss = Foreign.toJSString (getText ss)

        setAttribute key value content =
            go showEv setProps' children content
          where
            setProps' props =
                VirtualDom.setProperty key value props >> setProps props

        makePropertiesObject = do
            props <- VirtualDom.newProperties
            setProps props
            return props

        tagToVNode tag content = do
            props         <- makePropertiesObject
            innerChildren <- VirtualDom.newChildren
            go showEv (\_props -> return ()) innerChildren content
            let vnode = VirtualDom.vnode tag props innerChildren
            VirtualDom.pushChild vnode children

        leafToVNode tag = do
            props      <- makePropertiesObject
            noChildren <- VirtualDom.newChildren
            let vnode = VirtualDom.vnode tag props noChildren
            VirtualDom.pushChild vnode children

        textToVNode jsText =
            VirtualDom.pushChild (VirtualDom.text jsText) children




renderHtml :: Show ev => Markup ev -> IO (VirtualDom.VNode)
renderHtml html = do
    children <- renderAsVNodes (Foreign.toJSString . show) html
    props <- VirtualDom.newProperties
    return $! VirtualDom.vnode "div" props children
