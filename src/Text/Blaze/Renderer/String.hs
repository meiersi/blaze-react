
-- | A renderer that produces a native Haskell 'String', mostly meant for
-- debugging purposes.
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Renderer.String
    ( -- fromChoiceString
    -- , renderMarkup
      renderHtml
    ) where

import Data.List (isInfixOf)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SBC
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Blaze.Internal

-- | Escape predefined XML entities in a string
--
escapeMarkupEntities :: String  -- ^ String to escape
                   -> String  -- ^ String to append
                   -> String  -- ^ Resulting string
escapeMarkupEntities []     k = k
escapeMarkupEntities (c:cs) k = case c of
    '<'  -> '&' : 'l' : 't' : ';'             : escapeMarkupEntities cs k
    '>'  -> '&' : 'g' : 't' : ';'             : escapeMarkupEntities cs k
    '&'  -> '&' : 'a' : 'm' : 'p' : ';'       : escapeMarkupEntities cs k
    '"'  -> '&' : 'q' : 'u' : 'o' : 't' : ';' : escapeMarkupEntities cs k
    '\'' -> '&' : '#' : '3' : '9' : ';'       : escapeMarkupEntities cs k
    x    -> x                                 : escapeMarkupEntities cs k

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> String        -- ^ String to append
                 -> String        -- ^ Resulting string
fromChoiceString (Static s)     = getString s
fromChoiceString (String s)     = escapeMarkupEntities s
fromChoiceString (Text s)       = escapeMarkupEntities $ T.unpack s
fromChoiceString (ByteString s) = (SBC.unpack s ++)
fromChoiceString (PreEscaped x) = case x of
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

-- | Render some 'Markup' to an appending 'String'.
--
renderString
    :: Markup ev  -- ^ Markup to render
    -> String     -- ^ String to append
    -> String     -- ^ Resulting String
renderString =
    go id
  where
    go :: (String -> String) -> MarkupM ev b -> String -> String
    go attrs html = case html of
        MapActions _f content ->
            go attrs content
        OnEvent _ev content ->
            -- TODO (meiersi): add more details about the event handler registered.
            go attrs content
        Parent _ open close content ->
            getString open . attrs . ('>' :) . go id content . getString close
        CustomParent tag content ->
            ('<' :) . fromChoiceString tag . attrs . ('>' :) .
            go id content .
            ("</" ++) . fromChoiceString tag . ('>' :)
        Leaf _ begin end ->
            getString begin . attrs . getString end
        CustomLeaf tag close ->
            ('<' :) . fromChoiceString tag . attrs .
            (if close then (" />" ++) else ('>' :))
        AddAttribute _ key value h ->
            let attrs' = getString key . fromChoiceString value
                       . ('"' :) . attrs
            in go attrs' h
        AddBoolAttribute key value h ->
            let attrs' = (' ' :) . getString key . ("=\"" ++)
                       . ((if value then "true" else "false") ++) .  ('"' :) .  attrs
            in go attrs' h
        AddMapAttribute key value h ->
            let attrs' = (' ' :) . getString key . ("=\"" ++)
                       . ((M.foldMapWithKey (\key' value' ->
                            key' ++ ": " ++ show value' ++ "; ")
                          value) ++) .  ('"' :) .  attrs
            in go attrs' h
        AddCustomAttribute key value h ->
            let attrs' = (' ' :) . fromChoiceString key . ("=\"" ++)
                       . fromChoiceString value .  ('"' :) .  attrs
            in go attrs' h
        Content content -> fromChoiceString content
        Append h1 h2    -> go attrs h1 . go attrs h2
        Empty           -> id

-- | Render markup to a lazy 'String'.
--
renderMarkup :: Show ev => Markup ev -> String
renderMarkup html =
    renderString html ""

renderHtml :: Show ev => Markup ev -> String
renderHtml = renderMarkup
