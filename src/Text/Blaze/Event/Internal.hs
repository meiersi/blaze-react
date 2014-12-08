{-# LANGUAGE DeriveFunctor #-}

module Text.Blaze.Event.Internal
    ( EventHandler(..)

    , MouseButton(..)
    , MousePosition(..)
    , DomDelta(..)
    , DeltaValue(..)
    , File(..)
    ) where

import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime)

import           Text.Blaze.Event.Keycode  (Keycode)
import           Text.Blaze.Event.Charcode (Charcode)

-- | One specific and incomplete specifications of event-handlers geared
-- towards their use with ReactJS.
data EventHandler a
    = OnKeyDown  [Keycode]  (IO a)
    | OnKeyUp    [Keycode]  (IO a)
    | OnKeyPress [Charcode] (IO a)

    | OnFocus (IO a)
    | OnBlur  (IO a)

      -- NOTE (asayers): In ReactJS, I believe OnInput has the same semantics
      -- as OnChange, so I won't bother adding it here.
    | OnValueChange    (T.Text -> IO a)
    | OnCheckedChange  (Bool   -> IO a)
    | OnSelectedChange (Bool   -> IO a)
    | OnSubmit (IO a)

    | OnClick       [MouseButton] (MousePosition -> IO a)
    | OnDoubleClick [MouseButton] (MousePosition -> IO a)
    | OnMouseDown   [MouseButton] (MousePosition -> IO a)
    | OnMouseUp     [MouseButton] (MousePosition -> IO a)
    | OnMouseMove                 (MousePosition -> IO a)
    | OnMouseEnter                (MousePosition -> IO a)
    | OnMouseLeave                (MousePosition -> IO a)
    | OnMouseOver                 (MousePosition -> IO a)
    | OnMouseOut                  (MousePosition -> IO a)

    | OnScroll (Double -> IO a)
    | OnWheel (DomDelta -> IO a)

    -- TODO (asayers): Implement these
    -- OnCopy  ([File] -> IO a)
    -- OnCut   ([File] -> IO a)
    -- OnPaste ([File] -> IO a)

    -- TODO (asayers): Implement these.
    -- OnDrag      ([File] -> IO a)
    -- OnDragEnd   ([File] -> IO a)
    -- OnDragEnter ([File] -> IO a)
    -- OnDragExit  ([File] -> IO a)
    -- OnDragLeave ([File] -> IO a)
    -- OnDragOver  ([File] -> IO a)
    -- OnDragStart ([File] -> IO a)
    -- OnDrop      ([File] -> IO a)

    -- NOTE (asayers): These events require special initialization in React,
    -- and aren't supported by jQuery, so I'll omit them for now.
    -- OnTouchCancel (IO a)
    -- OnTouchEnd    (IO a)
    -- OnTouchMove   (IO a)
    -- OnTouchStart  (IO a)
    deriving (Functor)

data MouseButton = LeftButton | RightButton | MiddleButton deriving (Eq, Show)
data MousePosition = MousePosition
    { mpClientX :: Int
      -- ^ x-position relative to the upper-left corner of the viewport
    , mpClientY :: Int
      -- ^ y-position relative to the upper-left corner of the viewport
    , mpPageX   :: Int
      -- ^ x-position relative to the upper-left corner of the content-area
    , mpPageY   :: Int
      -- ^ y-position relative to the upper-left corner of the content-area
    , mpScreenX :: Int
      -- ^ x-position relative to the upper-left corner of the physical screen
    , mpScreenY :: Int
      -- ^ y-position relative to the upper-left corner of the physical screen
    } deriving (Eq, Show)

data DomDelta
    = PixelDelta DeltaValue
    | LineDelta  DeltaValue
    | PageDelta  DeltaValue
data DeltaValue = DeltaValue { deltaX :: Double, deltaY :: Double, deltaZ :: Double }

data File = File
    { fileName         :: T.Text
    , fileMimeType     :: T.Text
    , fileSize         :: Int    -- ^ Size of the blob in bytes
    , fileLastModified :: UTCTime
    , fileRead         :: IO BS.ByteString -- ^ Read the contents of the blob
    }

