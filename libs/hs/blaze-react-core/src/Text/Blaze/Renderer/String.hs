
-- | A renderer that produces a native Haskell 'String', mostly meant for
-- debugging purposes.
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Renderer.String
    ( -- fromChoiceString
    -- , renderMarkup
      renderHtml
      , renderHtmlPretty
    ) where

import qualified Data.ByteString         as S
import qualified Data.ByteString.Char8   as SBC
import qualified Data.HashMap.Strict     as HMS
import           Data.List               (isInfixOf)
import           Data.Monoid
import qualified Data.Text               as T

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
    :: Bool       -- ^ Whether the output should be pretty printed
    -> Markup ev  -- ^ Markup to render
    -> String     -- ^ String to append
    -> String     -- ^ Resulting String
renderString pretty markup append =
    -- If we are pretty printing, then we end up with one new line
    -- character in the beginning of the result string, so in that
    -- case we simply get rid of it with a tail.
    (if pretty then tail else id) (go id 0 markup append)
  where
    indentBy :: Int -> String -> String
    indentBy indent = if pretty then (('\n' : replicate (indent * 2) ' ') ++) else id

    go :: (String -> String) -> Int -> MarkupM ev b -> String -> String
    go attrs indent html = case html of
        MapActions _f content ->
            go attrs indent content
        OnEvent _ev content ->
            -- TODO (meiersi): add more details about the event handler registered.
            go attrs indent content
        Parent _ open close content ->
            indentBy indent . getString open . attrs . ('>' :) .
            go id (indent + 1) content .
            getString close
        CustomParent tag content ->
            indentBy indent . ('<' :) . fromChoiceString tag . attrs . ('>' :) .
            go id (indent + 1) content .
            ("</" ++) . fromChoiceString tag . ('>' :)
        Leaf _ begin end ->
            indentBy indent . getString begin . attrs . getString end
        CustomLeaf tag close ->
            indentBy indent . ('<' :) . fromChoiceString tag . attrs .
            (if close then (" />" ++) else ('>' :))
        AddAttribute _ key value h ->
            let attrs' = getString key . fromChoiceString value
                       . ('"' :) . attrs
            in go attrs' indent h
        AddBoolAttribute key value h ->
            let attrs' = (' ' :) . getString key . ("=\"" ++)
                       . ((if value then "true" else "false") ++) . ('"' :) .  attrs
            in go attrs' indent h
        AddCustomAttribute key value h ->
            let attrs' = (' ' :) . fromChoiceString key . ("=\"" ++)
                       . fromChoiceString value . ('"' :) .  attrs
            in go attrs' indent h
        AddObjectAttribute key object h ->
            let attrs' = (' ' :) . getString key . ("=\"" ++)
                       . ((T.unpack $ renderMap object) ++) . ('"' :) .  attrs
            in go attrs' indent h
        Content content -> fromChoiceString content
        Append h1 h2    -> go attrs indent h1 . go attrs indent h2
        Empty           -> id

-- | Render a text-text map to the form used in CSS. Eg:
--
-- >>> renderMap $ fromList [("foo", "bar"), ("baz", "qux")]
-- "foo: bar; baz: qux; "
--
--  TODO (asayers): Escape ':' and ';' characters.
renderMap :: HMS.HashMap T.Text T.Text -> T.Text
renderMap =
    HMS.foldlWithKey' (\acc key val -> acc <> key <> ": " <> val <> "; ") ""

-- | Render markup to a lazy 'String'.
--
renderMarkup :: Show ev => Bool -> Markup ev -> String
renderMarkup pretty html =
    renderString pretty html ""

renderHtml :: Show ev => Markup ev -> String
renderHtml = renderMarkup False

renderHtmlPretty :: Show ev => Markup ev -> String
renderHtmlPretty = renderMarkup True
