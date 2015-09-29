{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Blaze.Event.Internal
    ( EventHandler(..)
    , EventSelector(..)
    , SomeEventSelector(..)
    , Event(..)
    , eventDataToJson
    , SomeEvent(..)
    , someEventData
    , MouseButton(..)
    , MousePosition(..)
    , DomDelta(..)
    , DeltaValue(..)
    -- , File(..)
    ) where

import           Control.Monad

import           Data.Aeson                (ToJSON(..), FromJSON(..))
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Types          as Aeson
-- import qualified Data.ByteString           as BS
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
-- import           Data.Time.Clock           (UTCTime)
import           Data.Typeable             (Typeable )
import qualified Data.Vector               as V

import           GHC.Generics

import           Text.Blaze.Event.Keycode  (Keycode)
-- import           Text.Blaze.Event.Charcode (Charcode)


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

data MouseButton
    = LeftButton
    | RightButton
    | MiddleButton
    deriving (Eq, Show, Typeable, Generic, Enum, Bounded)

data MousePosition = MousePosition
    { mpClientX :: {-# UNPACK #-} !Int
      -- ^ x-position relative to the upper-left corner of the viewport
    , mpClientY :: {-# UNPACK #-} !Int
      -- ^ y-position relative to the upper-left corner of the viewport
    , mpPageX   :: {-# UNPACK #-} !Int
      -- ^ x-position relative to the upper-left corner of the content-area
    , mpPageY   :: {-# UNPACK #-} !Int
      -- ^ y-position relative to the upper-left corner of the content-area
    , mpScreenX :: {-# UNPACK #-} !Int
      -- ^ x-position relative to the upper-left corner of the physical screen
    , mpScreenY :: {-# UNPACK #-} !Int
      -- ^ y-position relative to the upper-left corner of the physical screen
    } deriving (Eq, Show, Generic, Typeable)

data DomDelta
    = PixelDelta !DeltaValue
    | LineDelta  !DeltaValue
    | PageDelta  !DeltaValue
    deriving (Eq, Show, Generic, Typeable)

data DeltaValue = DeltaValue
    { deltaX :: {-# UNPACK #-} !Double
    , deltaY :: {-# UNPACK #-} !Double
    , deltaZ :: {-# UNPACK #-} !Double
    } deriving (Eq, Show, Generic, Typeable)

{-
data File = File
    { fileName         ::                !T.Text
    , fileMimeType     ::                !T.Text
    , fileSize         :: {-# UNPACK #-} !Int
      -- ^ Size of the blob in bytes
    , fileLastModified ::                !UTCTime
    , fileRead         ::                !(IO BS.ByteString)
      -- ^ Read the contents of the blob.
      -- NOTE (SM): I'm not sure whether we can support this in a
      -- cross-platform way.
    }
-}

-- | Event-selectors that can be used in blaze-react.
data EventSelector evData where
    OnKeyDown  :: ![Keycode]  -> EventSelector Keycode
    -- OnKeyUp    :: ![Keycode]  -> EventSelector Keycode
    -- OnKeyPress :: ![Charcode] -> EventSelector Keycode

    -- OnFocus    :: EventSelector ()
    -- OnBlur     :: EventSelector ()

    -- -- NOTE (asayers): In ReactJS, I believe OnInput has the same semantics as
    -- -- OnChange, so I won't bother adding it here.
    -- -- NOTE (asayers): Using the 'selected' attribute of <option> elements
    -- -- seems to be discouraged in ReactJS (doing this throws a warning).
    -- -- Therefore I'm removing OnSelectedChange in favour of using OnValueChange
    -- -- on the <select> element.
    OnValueChange   :: EventSelector T.Text
    -- OnCheckedChange :: EventSelector Bool
    -- OnSubmit        :: EventSelector ()

    OnClick       :: ![MouseButton] -> EventSelector MousePosition
    -- OnDoubleClick :: ![MouseButton] -> EventSelector MousePosition
    -- OnMouseDown   :: ![MouseButton] -> EventSelector MousePosition
    -- OnMouseUp     :: ![MouseButton] -> EventSelector MousePosition
    -- OnMouseMove   ::                   EventSelector MousePosition
    -- OnMouseEnter  ::                   EventSelector MousePosition
    -- OnMouseLeave  ::                   EventSelector MousePosition
    -- OnMouseOver   ::                   EventSelector MousePosition
    -- OnMouseOut    ::                   EventSelector MousePosition

    -- OnScroll :: EventSelector Int
    -- OnWheel  :: EventSelector DomDelta


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

-- | An event that happened, i.e., a selector with corresponding event data.
data Event evData = Event !(EventSelector evData) !evData

-- | An Event handler converting events to actions of type 'act'.
data EventHandler act
    = forall evData.  EventHandler (EventSelector evData) (evData -> act)

-- | An event selector for some event data.
data SomeEventSelector where
    SomeEventSelector
        :: forall evData. EventSelector evData -> SomeEventSelector

-- | An event carrying some event data.
data SomeEvent where
    SomeEvent :: forall evData. Event evData -> SomeEvent


-- eq, show, and functor instances
----------------------------------

deriving instance Eq (EventSelector evData)
deriving instance Show (EventSelector evData)

instance Functor EventHandler where
    fmap f (EventHandler ev mkAct) = EventHandler ev (f . mkAct)



-- JSON instances
-----------------

instance ToJSON MousePosition
instance FromJSON MousePosition

eventDataToJson :: EventSelector evData -> evData -> Aeson.Value
eventDataToJson sel = case sel of
    OnKeyDown _   -> toJSON
    OnValueChange -> toJSON
    OnClick _     -> toJSON

eventDataParseJson
    :: EventSelector evData -> Aeson.Value -> Aeson.Parser evData
eventDataParseJson sel = case sel of
    OnKeyDown _   -> parseJSON
    OnValueChange -> parseJSON
    OnClick _     -> parseJSON

instance ToJSON (Event evData) where
    toJSON (Event sel evData) = toJSON (toJSON sel, eventDataToJson sel evData)

instance ToJSON SomeEvent where
    toJSON (SomeEvent ev) = toJSON ev

instance FromJSON SomeEvent where
    parseJSON = Aeson.withArray "SomeEvent" $ \arr -> do
        -- parse event selector
        SomeEventSelector sel
            <- maybe (fail "Expected event selector.") parseJSON (arr V.!? 0)

        -- parse event-data dependent on the selector that was parsed
        evData <- maybe (fail "Expected event data.")
                        (eventDataParseJson sel)
                        (arr V.!? 1)
        return $ SomeEvent (Event sel evData)



instance ToJSON (EventSelector evData) where
    toJSON sel = case sel of
        OnKeyDown keys  -> tagged 0 [toJSON keys]
        OnValueChange   -> tagged 5 []
        OnClick buttons -> tagged 8 [toJSON buttons]
      where
        tagged :: Int -> [Aeson.Value] -> Aeson.Value
        tagged tag args = toJSON (toJSON tag : args)

instance ToJSON SomeEventSelector where
    toJSON (SomeEventSelector ev) = toJSON ev

instance FromJSON SomeEventSelector where
    parseJSON = Aeson.withArray "EventSelector" $ \arr -> do
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

            lift0 :: a -> Aeson.Parser a
            lift0 v = check 1 (Just (return v))

            lift1 :: FromJSON a => (a -> b) -> Aeson.Parser b
            lift1 constr = check 2 (fmap constr <$> pos 1)

        case (tag :: Int) of
          0 -> lift1 (SomeEventSelector . OnKeyDown)
          5 -> lift0 (SomeEventSelector OnValueChange)
          8 -> lift1 (SomeEventSelector . OnClick)
          _ -> fail $ "Unexpected tag " <> show tag


instance ToJSON MouseButton where
    toJSON = toJSON . fromEnum

instance FromJSON MouseButton where
    parseJSON = \val -> do
        i <- parseJSON val
        guard (lb <= i && i <= ub)
        return $ toEnum i
      where
        lb = fromEnum (minBound :: MouseButton)
        ub = fromEnum (maxBound :: MouseButton)



------------------------------------------------------------------------------
-- Event-data extraction
------------------------------------------------------------------------------


someEventData :: SomeEvent -> EventSelector evData -> Maybe evData
someEventData (SomeEvent (Event sel evData)) sel' =
    case (sel, sel') of
      (OnKeyDown x,   OnKeyDown x')  -> ifEqual x x' evData
      (OnValueChange, OnValueChange) -> Just evData
      (OnClick x,     OnClick x')    -> ifEqual x x' evData
      _                              -> Nothing
  where
    ifEqual :: Eq a => a -> a -> b -> Maybe b
    ifEqual a a' b = guard (a == a') >> return b
