
-- | A renderer that produces a native Haskell 'String', mostly meant for
-- debugging purposes.
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Renderer.String
    ( -- fromChoiceString
    -- , renderMarkup
      renderHtml
    ) where

import qualified Data.HashMap.Strict     as HMS
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
    go :: (String -> String) -> Markup ev -> String -> String
    go attrs html = case html of
        OnEvent _ev content ->
            go attrs content
        Parent tag content ->
            ('<' :) . fromChoiceString tag . attrs . ('>' :) .
            go id content .
            ("</" ++) . fromChoiceString tag . ('>' :)
        Leaf tag close ->
            ('<' :) . fromChoiceString tag . attrs .
            (if close then (" />" ++) else ('>' :))
        AddAttribute key value h ->
            let attrs' = (' ' :) . fromChoiceString key . ("=\"" ++) . fromChoiceString value
                       . ('"' :) . attrs
            in go attrs' h
        AddBoolAttribute key value h ->
            let attrs' = (' ' :) . fromChoiceString key . ("=\"" ++)
                       . ((if value then "true" else "false") ++) .  ('"' :) .  attrs
            in go attrs' h
        AddCustomAttribute key value h ->
            let attrs' = (' ' :) . fromChoiceString key . ("=\"" ++)
                       . fromChoiceString value .  ('"' :) .  attrs
            in go attrs' h
        AddObjectAttribute key object h ->
            let attrs' = (' ' :) . fromChoiceString key . ("=\"" ++)
                       . ((T.unpack $ renderMap object) ++)
            in go attrs' h
        Content content -> fromChoiceString content
        Append h1 h2    -> go attrs h1 . go attrs h2
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
renderMarkup :: Markup ev -> String
renderMarkup html =
    renderString html ""

renderHtml :: Markup ev -> String
renderHtml = renderMarkup
