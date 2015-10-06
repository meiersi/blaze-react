{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | Generic abstractions for markup languages like Html and SVG.
module Blaze.React.Markup
    (
      -- * Important types.
      Markup
    , Tag
    , Attribute
    , AttributeValue

      -- * Creating attributes.
    , dataAttribute
    , customAttribute

      -- * Converting values to Markup.
    , ToMarkup (..)

      -- * Creating tags.
    , textTag
    , stringTag

      -- * Converting values to attribute values.
    , ToValue (..)

      -- * Setting attributes
    , (!)
    , (!?)

    ) where


import Blaze.React.Markup.Internal

import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)

import Data.Text (Text)
import qualified Data.Text.Lazy as LT


-- | Class allowing us to use a single function for Markup values
--
class ToMarkup a where
    -- | Convert a value to Markup.
    --
    toMarkup :: a -> Markup ev

instance ToMarkup Text where
    toMarkup = text
    {-# INLINE toMarkup #-}

instance ToMarkup LT.Text where
    toMarkup = lazyText
    {-# INLINE toMarkup #-}

instance ToMarkup String where
    toMarkup = string
    {-# INLINE toMarkup #-}

instance ToMarkup Int where
    toMarkup = string . show
    {-# INLINE toMarkup #-}

instance ToMarkup Int32 where
    toMarkup = string . show
    {-# INLINE toMarkup #-}

instance ToMarkup Int64 where
    toMarkup = string . show
    {-# INLINE toMarkup #-}

instance ToMarkup Char where
    toMarkup = string . return
    {-# INLINE toMarkup #-}

instance ToMarkup Bool where
    toMarkup = string . show
    {-# INLINE toMarkup #-}

instance ToMarkup Integer where
    toMarkup = string . show
    {-# INLINE toMarkup #-}

instance ToMarkup Float where
    toMarkup = string . show
    {-# INLINE toMarkup #-}

instance ToMarkup Double where
    toMarkup = string . show
    {-# INLINE toMarkup #-}

instance ToMarkup Word where
    toMarkup = string . show
    {-# INLINE toMarkup #-}

instance ToMarkup Word32 where
    toMarkup = string . show
    {-# INLINE toMarkup #-}

instance ToMarkup Word64 where
    toMarkup = string . show
    {-# INLINE toMarkup #-}

-- | Class allowing us to use a single function for attribute values
--
class ToValue a where
    -- | Convert a value to an attribute value
    --
    toValue :: a -> AttributeValue

instance ToValue AttributeValue where
    toValue = id
    {-# INLINE toValue #-}

instance ToValue Text where
    toValue = textValue
    {-# INLINE toValue #-}

instance ToValue LT.Text where
    toValue = lazyTextValue
    {-# INLINE toValue #-}

instance ToValue String where
    toValue = stringValue
    {-# INLINE toValue #-}

instance ToValue Int where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Int32 where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Int64 where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Char where
    toValue = stringValue . return
    {-# INLINE toValue #-}

instance ToValue Bool where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Integer where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Float where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Double where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Word where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Word32 where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Word64 where
    toValue = stringValue . show
    {-# INLINE toValue #-}
