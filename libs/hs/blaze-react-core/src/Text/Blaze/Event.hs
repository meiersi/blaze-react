{-# LANGUAGE DeriveFunctor #-}

module Text.Blaze.Event
    ( MouseButton(..)
    , MousePosition(..)
    , Keycode
    , Charcode
    , EventHandler
    , SomeEvent
    , SomeEventSelector

      -- ** Mapping over all actions
    , mapActions

      -- ** Keyboard events
    , onKeyDown
    -- , onKeyUp
    -- , onKeyPress

    --   -- ** Focus events
    -- , onFocus
    -- , onBlur

    --   -- ** Form events
    -- , onValueChange
    -- , onCheckedChange
    -- , onSubmit

    --   -- ** Mouse events
    , onClick           , onClick'
    -- , onDoubleClick     , onDoubleClick'
    -- , onMouseDown       , onMouseDown'
    -- , onMouseUp         , onMouseUp'
    -- , onMouseMove
    -- , onMouseEnter
    -- , onMouseLeave
    -- , onMouseOver
    -- , onMouseOut

    --   -- ** UI Events
    -- , onScroll

    --   -- ** Wheel Events
    -- , onWheel

    ) where

-- import qualified Data.Text                 as T

import           Text.Blaze.Event.Keycode  (Keycode)
import           Text.Blaze.Event.Charcode (Charcode)
import           Text.Blaze.Event.Internal
import           Text.Blaze.Internal       (Attribute(..), Markup(..))


mapActions :: (act -> act') -> Markup (EventHandler act) -> Markup (EventHandler act')
mapActions = fmap . fmap

-- Keyboard events
-------------------------------------------------------------------------------

-- | The user has pressed a physical key while the target element was focused.
-- The callback will only be called if this key matches the one of the
-- specified Keycodes.
onKeyDown :: [Keycode] -> (Keycode -> act) -> Attribute (EventHandler act)
onKeyDown = onEvent . OnKeyDown

{-
-- | The user has released a phyiscal key while the target element was focused.
-- The callback will only be called if this key matches the one of the
-- specified Keycodes.
onKeyUp :: [Keycode] -> act -> Attribute act
onKeyUp keys = onKeyUpM keys . return

-- | A version of 'onKeyUp' which allows I/O to be performed in the callback.
onKeyUpM :: [Keycode] -> IO act -> Attribute act
onKeyUpM keys = onEvent . OnKeyUp keys

-- | The user has input some ASCII character while the target element was
-- focused. The callback will only be called if this character matches one of
-- the specified Charcodes.
onKeyPress :: [Charcode] -> act -> Attribute act
onKeyPress chars = onKeyPressM chars . return

-- | A version of 'onKeyPress' which allows I/O to be performed in the callback.
onKeyPressM :: [Charcode] -> IO act -> Attribute act
onKeyPressM chars = onEvent . OnKeyPress chars

-- Focus events
-------------------------------------------------------------------------------

-- | The focus has moved to the target element.
onFocus :: act -> Attribute act
onFocus = onFocusM . return

-- | A version of 'onFocus' which allows I/O to be performed in the callback.
onFocusM :: IO act -> Attribute act
onFocusM = onEvent . OnFocus

-- | The focus has left the target element.
onBlur :: act -> Attribute act
onBlur = onBlurM . return

-- | A version of 'onBlur' which allows I/O to be performed in the callback.
onBlurM :: IO act -> Attribute act
onBlurM = onEvent . OnBlur

-- Form events
-------------------------------------------------------------------------------

-- | The 'value' property of the target element has changed. The new value is
-- passed as a parameter to the callback. This handler is supported for
-- <input>, <textarea>, and <select> elements.
onValueChange :: (T.Text -> act) -> Attribute act
onValueChange mkAct = onValueChangeM $ return . mkAct

-- | A version of 'onValueChange' which allows I/O to be performed in the
-- callback.
onValueChangeM :: (T.Text -> IO act) -> Attribute act
onValueChangeM = onEvent . OnValueChange

-- | The 'checked' property of the target element has changed. This handler is
-- supported for <input> elements of type 'checkbox' or 'radio'.
onCheckedChange :: (Bool -> act) -> Attribute act
onCheckedChange mkAct = onCheckedChangeM $ return . mkAct

-- | A version of 'onCheckedChange' which allows I/O to be performed in the
-- callback.
onCheckedChangeM :: (Bool -> IO act) -> Attribute act
onCheckedChangeM = onEvent . OnCheckedChange

-- | The user has submitted the target form. This handler is supported for
-- <form> elements.
onSubmit :: act -> Attribute act
onSubmit = onSubmitM . return

-- | A version of 'onSubmit' which allows I/O to be performed in the callback.
onSubmitM :: IO act -> Attribute act
onSubmitM = onEvent . OnSubmit
-}

-- Mouse events
-------------------------------------------------------------------------------

-- | A simplified version of 'onClick' which watches for the 'LeftButton' only
-- and ignores the cursor position.
onClick' :: act -> Attribute (EventHandler act)
onClick' = onClick [LeftButton] . const

-- | The user has pressed and released a mouse button while the cursor was
-- positioned over the target element. The callback will only be called if this
-- mouse button matches one of the specified buttons. The mouse position and
-- the pressed button at the time the event was fired is passed as a parameter
-- to the callback.
onClick :: [MouseButton] -> (MousePosition -> act) -> Attribute (EventHandler act)
onClick btns mkAct = onEvent (OnClick btns) mkAct

{-

-- | A version of 'onClick' which allows I/O to be performed in the callback.
onClickM :: [MouseButton] -> (MousePosition -> IO act) -> Attribute act
onClickM btns = onEvent . OnClick btns

-- | A simplified version of 'onDoubleClick' which watches for the 'LeftButton'
-- only and ignores the cursor position.
onDoubleClick' :: act -> Attribute act
onDoubleClick' = onDoubleClick [LeftButton] . const

-- | The user has pressed and released a mouse button twice in quick succession
-- while the cursor was positioned over the target element. The callback will
-- only be called if this mouse button matches one of the specified buttons.
-- The mouse position at the time the event was fired is passed as a parameter
-- to the callback.
onDoubleClick :: [MouseButton] -> (MousePosition -> act) -> Attribute act
onDoubleClick btns mkAct = onDoubleClickM btns $ return . mkAct

-- | A version of 'onDoubleClick' which allows I/O to be performed in the
-- callback.
onDoubleClickM :: [MouseButton] -> (MousePosition -> IO act) -> Attribute act
onDoubleClickM btns = onEvent . OnDoubleClick btns

-- | A simplified version of 'onMouseDown' which watches for the 'LeftButton'
-- only and ignores the cursor position.
onMouseDown' :: act -> Attribute act
onMouseDown' = onMouseDown [LeftButton] . const

-- | The user has pressed a mouse button while the cursor was positioned over
-- the target element. The callback will only be called if this mouse button
-- matches one of the specified buttons. The mouse position at the time the
-- event was fired is passed as a parameter to the callback.
onMouseDown :: [MouseButton] -> (MousePosition -> act) -> Attribute act
onMouseDown btns mkAct = onMouseDownM btns $ return . mkAct

-- | A version of 'onMouseDown' which allows I/O to be performed in the
-- callback.
onMouseDownM :: [MouseButton] -> (MousePosition -> IO act) -> Attribute act
onMouseDownM btns = onEvent . OnMouseDown btns

-- | A simplified version of 'onMouseUp' which watches for the 'LeftButton'
-- only and ignores the cursor position.
onMouseUp' :: act -> Attribute act
onMouseUp' = onMouseUp [LeftButton] . const

-- | The user has released a mouse button while the cursor was positioned over
-- the target element. The callback will only be called if this mouse button
-- matches one of the specified buttons. The mouse position at the time the
-- event was fired is passed as a parameter to the callback.
onMouseUp :: [MouseButton] -> (MousePosition -> act) -> Attribute act
onMouseUp btns mkAct = onMouseUpM btns $ return . mkAct

-- | A version of 'onMouseUp' which allows I/O to be performed in the callback.
onMouseUpM :: [MouseButton] -> (MousePosition -> IO act) -> Attribute act
onMouseUpM btns = onEvent . OnMouseUp btns

-- | The mouse cursor has moved while positioned over the target element. The
-- mouse position at the time the event was fired is passed as a parameter to
-- the callback.
onMouseMove :: (MousePosition -> act) -> Attribute act
onMouseMove mkAct = onMouseMoveM $ return . mkAct

-- | A version of 'onMouseMove' which allows I/O to be performed in the
-- callback.
onMouseMoveM :: (MousePosition -> IO act) -> Attribute act
onMouseMoveM = onEvent . OnMouseMove

-- | The mouse cursor has entered the region occupied by the target element.
-- The mouse position at the time the event was fired is passed as a parameter
-- to the callback.
onMouseEnter :: (MousePosition -> act) -> Attribute act
onMouseEnter mkAct = onMouseEnterM $ return . mkAct

-- | A version of 'onMouseEnter' which allows I/O to be performed in the
-- callback.
onMouseEnterM :: (MousePosition -> IO act) -> Attribute act
onMouseEnterM = onEvent . OnMouseEnter

-- | The mouse cursor has left the region occupied by the target element. The
-- mouse position at the time the event was fired is passed as a parameter to
-- the callback.
onMouseLeave :: (MousePosition -> act) -> Attribute act
onMouseLeave mkAct = onMouseLeaveM $ return . mkAct

-- | A version of 'onMouseLeave' which allows I/O to be performed in the
-- callback.
onMouseLeaveM :: (MousePosition -> IO act) -> Attribute act
onMouseLeaveM = onEvent . OnMouseLeave

-- | The mouse cursor has entered the region where the target element is
-- visible. The mouse position at the time the event was fired is passed as a
-- parameter to the callback.
onMouseOver :: (MousePosition -> act) -> Attribute act
onMouseOver mkAct = onMouseOverM $ return . mkAct

-- | A version of 'onMouseOver' which allows I/O to be performed in the
-- callback.
onMouseOverM :: (MousePosition -> IO act) -> Attribute act
onMouseOverM = onEvent . OnMouseOver

-- | The mouse cursor has left the region where the target element is visible.
-- The mouse position at the time the event was fired is passed as a parameter
-- to the callback.
onMouseOut :: (MousePosition -> act) -> Attribute act
onMouseOut mkAct = onMouseOutM $ return . mkAct

-- | A version of 'onMouseOut' which allows I/O to be performed in the
-- callback.
onMouseOutM :: (MousePosition -> IO act) -> Attribute act
onMouseOutM = onEvent . OnMouseOut

-- UI events
-------------------------------------------------------------------------------

-- | The the scroll-position of the page has changed. The amount by which it
-- has changed (in lines) is passed as a parameter to the callback.
onScroll :: (Int -> act) -> Attribute act
onScroll mkAct = onScrollM $ return . mkAct

-- | A version of 'onScroll' which allows I/O to be performed in the callback.
onScrollM :: (Int -> IO act) -> Attribute act
onScrollM = onEvent . OnScroll

-- Wheel events
-------------------------------------------------------------------------------

-- | The user has moved the scroll-wheel. The amount by which the scroll
-- position of an infinitely large page is affected is passed as a parameter to
-- the callback.
onWheel :: (DomDelta -> act) -> Attribute act
onWheel mkAct = onWheelM $ return . mkAct

-- | A version of 'onWheel' which allows I/O to be performed in the callback.
onWheelM :: (DomDelta -> IO act) -> Attribute act
onWheelM = onEvent . OnWheel


-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------
-}

-- | Register an event handler.
onEvent :: EventSelector eventData -> (eventData -> act) -> Attribute (EventHandler act)
onEvent ev mkAct = Attribute (OnEvent (EventHandler ev mkAct))
{-# INLINE onEvent #-}

