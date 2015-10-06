
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, Rank2Types,
             FlexibleInstances, ExistentialQuantification,
             DeriveDataTypeable, MultiParamTypeClasses, DeriveFunctor,
             DeriveFoldable, DeriveTraversable,
             FunctionalDependencies #-}
-- | Internal types for representing markup-like languages.
--
-- While this module is exported, usage of it is not recommended, unless you
-- know what you are doing. This module might undergo changes at any time.
--
module Blaze.React.Markup.Internal
    (
      -- * Important types.
      ChoiceString (..)
    , StaticString (..)
    , Markup(..)
    , Tag
    , Attribute (..)
    , AttributeValue

      -- * Creating custom tags and attributes.
    , customParent
    , customLeaf
    , attribute
    , boolAttribute
    , dataAttribute
    , customAttribute
    , objectAttribute

      -- * Converting values to Markup.
    , text
    , lazyText
    , string

      -- * Converting values to tags.
    , textTag
    , stringTag

      -- * Converting values to attribute values.
    , textValue
    , lazyTextValue
    , stringValue

      -- * Setting attributes
    , Attributable
    , (!)
    , (!?)

      -- * Modifying Markup elements
    , external

      -- * Querying Markup elements
    , null
    ) where

import           Control.Applicative
import           Control.Monad.Writer

import           Data.Aeson                   (ToJSON(..), FromJSON(..))
import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Types             as Aeson (Parser)
import qualified Data.ByteString              as B
import qualified Data.HashMap.Strict          as HMS
import qualified Data.List                    as List
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy               as LT
import           Data.Typeable                (Typeable)
import qualified Data.Vector                  as V

import           GHC.Exts                     (IsString (..))

import           Prelude                      hiding (null)


-- | A static string that supports efficient output to all possible backends.
--
data StaticString = StaticString
    { getString         :: String -> String  -- ^ Appending haskell string
    , getUtf8ByteString :: B.ByteString      -- ^ UTF-8 encoded bytestring
    , getText           :: Text              -- ^ Text value
    }

-- 'StaticString's should only be converted from string literals, as far as I
-- can see.
--
instance IsString StaticString where
    fromString s = let t = T.pack s
                   in StaticString (s ++) (T.encodeUtf8 t) t

-- | A string denoting input from different string representations.
--
data ChoiceString
    -- | Static data
    = Static {-# UNPACK #-} !StaticString
    -- | A Haskell String
    | String String
    -- | A Text value
    | Text Text
    -- | Concatenation
    | AppendChoiceString ChoiceString ChoiceString
    -- | Empty string
    | EmptyChoiceString

instance Monoid ChoiceString where
    mempty = EmptyChoiceString
    {-# INLINE mempty #-}
    mappend = AppendChoiceString
    {-# INLINE mappend #-}

instance IsString ChoiceString where
    fromString = String
    {-# INLINE fromString #-}

-- | The core Markup datatype. The 'ev' type-parameter tracks the type of
-- events that can be raised when this Markup is rendered.
--
data Markup ev
    = -- | Mark a part of the tree with an event.
      OnEvent ev (Markup ev)
      -- | Custom parent
    | Parent ChoiceString (Markup ev)
      -- | Custom leaf
    | Leaf ChoiceString Bool
      -- | HTML content
    | Content ChoiceString
      -- | Concatenation of two HTML pieces
    | Append (Markup ev) (Markup ev)
      -- | Add an attribute to the inner HTML. Raw key, key, value, HTML to
      -- receive the attribute.
    | AddAttribute ChoiceString ChoiceString (Markup ev)
      -- | Add a boolean attribute.
    | AddBoolAttribute ChoiceString Bool (Markup ev)
      -- | Add a custom attribute to the inner HTML.
    | AddCustomAttribute ChoiceString ChoiceString (Markup ev)
      -- | Add an attribute containing a text-text map to the inner HTML.
    | AddObjectAttribute ChoiceString (HMS.HashMap T.Text T.Text) (Markup ev)
      -- | Empty HTML.
    | Empty
    deriving (Functor, Foldable, Traversable, Typeable)


instance Monoid (Markup ev) where
    mempty = Empty
    {-# INLINE mempty #-}
    mappend = Append
    {-# INLINE mappend #-}
    mconcat = foldr Append Empty
    {-# INLINE mconcat #-}

instance IsString (Markup ev) where
    fromString = Content . fromString
    {-# INLINE fromString #-}


-- | Type for an HTML tag. This can be seen as an internal string type used by
-- BlazeMarkup.
--
newtype Tag = Tag { unTag :: StaticString }
    deriving (IsString)

-- | Type for an attribute.
--
newtype Attribute ev = Attribute (Markup ev -> Markup ev)

instance Monoid (Attribute ev) where
    mempty                            = Attribute id
    Attribute f `mappend` Attribute g = Attribute (g . f)

-- | The type for the value part of an attribute.
--
newtype AttributeValue = AttributeValue { unAttributeValue :: ChoiceString }
    deriving (IsString, Monoid)


-- custom tags
--------------

-- | Create a custom parent element
customParent :: Tag       -- ^ Element tag
             -> Markup ev -- ^ Content
             -> Markup ev -- ^ Resulting markup
customParent tag = Parent (Static $ unTag tag)

-- | Create a custom leaf element
customLeaf :: Tag       -- ^ Element tag
           -> Bool      -- ^ Close the leaf?
           -> Markup ev -- ^ Resulting markup
customLeaf tag = Leaf (Static $ unTag tag)

-- | Create an HTML attribute that can be applied to an HTML element later using
-- the '!' operator.
--
attribute :: Tag             -- ^ Key
          -> AttributeValue  -- ^ Value for the HTML attribute.
          -> Attribute ev    -- ^ Resulting HTML attribute.
attribute rawKey value = Attribute $
    AddAttribute (Static $ unTag rawKey) (unAttributeValue value)
{-# INLINE attribute #-}

boolAttribute :: Tag -> Bool -> Attribute ev
boolAttribute rawKey value =
    Attribute $ AddBoolAttribute (Static $ unTag rawKey) value

-- | From HTML 5 onwards, the user is able to specify custom data attributes.
--
-- An example:
--
-- > <p data-foo="bar">Hello.</p>
--
-- We support this in BlazeMarkup using this funcion. The above fragment could
-- be described using BlazeMarkup with:
--
-- > p ! dataAttribute "foo" "bar" $ "Hello."
--
dataAttribute :: Tag             -- ^ Name of the attribute.
              -> AttributeValue  -- ^ Value for the attribute.
              -> Attribute ev    -- ^ Resulting HTML attribute.
dataAttribute tag value = Attribute $ AddCustomAttribute
    (Static "data-" `mappend` Static (unTag tag))
    (unAttributeValue value)
{-# INLINE dataAttribute #-}

-- | Create a custom attribute. This is not specified in the HTML spec, but some
-- JavaScript libraries rely on it.
--
-- An example:
--
-- > <select dojoType="select">foo</select>
--
-- Can be produced using:
--
-- > select ! customAttribute "dojoType" "select" $ "foo"
--
customAttribute :: Tag             -- ^ Name of the attribute
                -> AttributeValue  -- ^ Value for the attribute
                -> Attribute ev    -- ^ Resulting HTML attribtue
customAttribute tag value = Attribute $
    AddCustomAttribute (Static $ unTag tag) (unAttributeValue value)
{-# INLINE customAttribute #-}

-- | Create an attribute with a text-text map as value.
--
-- This is not specified in the HTML spec, but some
-- JavaScript libraries like react rely on it.
--
-- See: http://facebook.github.io/react/tips/inline-styles.html for
-- why this is needed.
objectAttribute :: Tag                       -- ^ Name of the attribute
                -> HMS.HashMap T.Text T.Text -- ^ Value of the attribute
                -> Attribute ev              -- ^ Resulting HTML attribtu
objectAttribute key object = Attribute $
    AddObjectAttribute (Static $ unTag key) object
{-# INLINE objectAttribute #-}

-- | Render text. Functions like these can be used to supply content in HTML.
--
text :: Text       -- ^ Text to render.
     -> Markup ev  -- ^ Resulting HTML fragment.
text = Content . Text
{-# INLINE text #-}

-- | A variant of 'text' for lazy 'LT.Text'.
--
lazyText :: LT.Text    -- ^ Text to insert
         -> Markup ev  -- ^ Resulting HTML fragment
lazyText = Content . mconcat . map Text . LT.toChunks
{-# INLINE lazyText #-}

-- | Create an HTML snippet from a 'String'.
--
string :: String    -- ^ String to insert.
       -> Markup ev -- ^ Resulting HTML fragment.
string = Content . String
{-# INLINE string #-}

-- | Create a 'Tag' from some 'Text'.
--
textTag :: Text  -- ^ Text to create a tag from
        -> Tag   -- ^ Resulting tag
textTag t = Tag $ StaticString (T.unpack t ++) (T.encodeUtf8 t) t

-- | Create a 'Tag' from a 'String'.
--
stringTag :: String  -- ^ String to create a tag from
          -> Tag     -- ^ Resulting tag
stringTag = Tag . fromString

-- | Render an attribute value from 'Text'.
--
textValue :: Text            -- ^ The actual value.
          -> AttributeValue  -- ^ Resulting attribute value.
textValue = AttributeValue . Text
{-# INLINE textValue #-}

-- | A variant of 'textValue' for lazy 'LT.Text'
--
lazyTextValue :: LT.Text         -- ^ The actual value
              -> AttributeValue  -- ^ Resulting attribute value
lazyTextValue = mconcat . map textValue . LT.toChunks
{-# INLINE lazyTextValue #-}

-- | Create an attribute value from a 'String'.
--
stringValue :: String -> AttributeValue
stringValue = AttributeValue . String
{-# INLINE stringValue #-}

-- | Used for applying attributes. You should not define your own instances of
-- this class.
class Attributable h ev | h -> ev where
    -- | Apply an attribute to an element.
    --
    -- Example:
    --
    -- > img ! src "foo.png"
    --
    -- Result:
    --
    -- > <img src="foo.png" />
    --
    -- This can be used on nested elements as well.
    --
    -- Example:
    --
    -- > p ! style "float: right" $ "Hello!"
    --
    -- Result:
    --
    -- > <p style="float: right">Hello!</p>
    --
    (!) :: h -> Attribute ev -> h

instance Attributable (Markup ev) ev where
    h ! (Attribute f) = f h
    {-# INLINE (!) #-}

instance Attributable (Markup ev -> Markup ev) ev where
    h ! f = (! f) . h
    {-# INLINE (!) #-}

-- | Shorthand for setting an attribute depending on a conditional.
--
-- Example:
--
-- > p !? (isBig, A.class "big") $ "Hello"
--
-- Gives the same result as:
--
-- > (if isBig then p ! A.class "big" else p) "Hello"
--
(!?) :: Attributable h ev => h -> (Bool, Attribute ev) -> h
(!?) h (c, a) = if c then h ! a else h

-- | Mark HTML as external data. External data can be:
--
-- * CSS data in a @<style>@ tag;
--
-- * Script data in a @<script>@ tag.
--
-- This function is applied automatically when using the @style@ or @script@
-- combinators.
--
external :: Markup ev -> Markup ev
external = error "Text.Blaze.Markup: implement external"
{-# INLINABLE external #-}

-- | Check if a 'Markup' value is completely empty (renders to the empty
-- string).
null :: Markup ev -> Bool
null markup = case markup of
    OnEvent _ c              -> null c
    Parent _ _               -> False
    Leaf _ _                 -> False
    Content c                -> emptyChoiceString c
    Append c1 c2             -> null c1 && null c2
    AddAttribute _ _ c       -> null c
    AddBoolAttribute _ _ c   -> null c
    AddCustomAttribute _ _ c -> null c
    AddObjectAttribute _ _ c -> null c
    Empty                    -> True
  where
    emptyChoiceString cs = case cs of
        Static ss                -> emptyStaticString ss
        String s                 -> List.null s
        Text t                   -> T.null t
        AppendChoiceString c1 c2 -> emptyChoiceString c1 && emptyChoiceString c2
        EmptyChoiceString        -> True

    emptyStaticString = B.null . getUtf8ByteString


------------------------------------------------------------------------------
-- Serialization
------------------------------------------------------------------------------

choiceStringToString :: ChoiceString -> String
choiceStringToString =
    go []
  where
    go k cs = case cs of
        Static ss                -> getString ss k
        String s                 -> s ++ k
        Text t                   -> T.unpack t ++ k
        AppendChoiceString c1 c2 -> go (go k c2) c1
        EmptyChoiceString        -> k

instance ToJSON ChoiceString where
    -- NOTE (SM): we use this chance to flatten the string.
    toJSON = toJSON . choiceStringToString

instance FromJSON ChoiceString where
    parseJSON val = Text <$> parseJSON val

instance ToJSON ev => ToJSON (Markup ev) where
    toJSON markup = case markup of
        OnEvent ev c             -> tagged 0 [toJSON ev, toJSON c]
        Parent tag c             -> tagged 1 [toJSON tag, toJSON c]
        Leaf tag selfClosing     -> tagged 2 [toJSON tag, toJSON selfClosing]
        Content c                -> tagged 3 [toJSON c]
        Append c1 c2             -> tagged 4 [toJSON c1, toJSON c2]
        AddAttribute k v c       -> tagged 5 [toJSON k, toJSON v, toJSON c]
        AddBoolAttribute k v c   -> tagged 6 [toJSON k, toJSON v, toJSON c]
        AddCustomAttribute k v c -> tagged 7 [toJSON k, toJSON v, toJSON c]
        AddObjectAttribute k v c -> tagged 8 [toJSON k, toJSON v, toJSON c]
        Empty                    -> tagged 9 []
      where
        tagged :: Int -> [Aeson.Value] -> Aeson.Value
        tagged tag args = toJSON (toJSON tag : args)

instance FromJSON ev => FromJSON (Markup ev) where
    parseJSON = Aeson.withArray "Markup" $ \arr -> do
        tag <- maybe (fail "Expected tag.") parseJSON (arr V.!? 0)
        let pos :: FromJSON a => Int -> Maybe (Aeson.Parser a)
            pos i = parseJSON <$> (arr V.!? i)

            check :: Int -> Maybe (Aeson.Parser a) -> Aeson.Parser a
            check l mbM
              | l == V.length arr =
                  maybe (fail "Failed inner parse.") id mbM
              | otherwise     = fail $
                  "Expected length " <> show l <>
                  ", got " <> show (V.length arr) <> "."

            lift1 :: FromJSON a => (a -> b) -> Aeson.Parser b
            lift1 constr = check 2 (fmap constr <$> pos 1)

            lift2 :: (FromJSON a, FromJSON b) => (a -> b -> c) -> Aeson.Parser c
            lift2 constr = check 3 (liftA2 constr <$> pos 1 <*> pos 2)

            lift3 :: (FromJSON a, FromJSON b, FromJSON c)
                  => (a -> b -> c -> d) -> Aeson.Parser d
            lift3 constr = check 4 (liftA3 constr <$> pos 1 <*> pos 2 <*> pos 3)

        case (tag :: Int) of
          0 -> lift2 OnEvent
          1 -> lift2 Parent
          2 -> lift2 Leaf
          3 -> lift1 Content
          4 -> lift2 Append
          5 -> lift3 AddAttribute
          6 -> lift3 AddBoolAttribute
          7 -> lift3 AddCustomAttribute
          8 -> lift3 AddObjectAttribute
          9 -> check 1 (Just (return Empty))
          _ -> fail $ "Unexpected tag " <> show tag

