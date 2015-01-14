{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A demo application designed to showcase all the different event handlers
-- currently implemented.
module Blaze.React.Examples.EventTest
    ( app
    ) where

import           Blaze.React

import           Control.Lens                         (makeLenses, (%=), (.=))

import qualified Data.Text                            as T
import           Data.Typeable                        (Typeable)

import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Event.Keycode             as Keycode
import qualified Text.Blaze.Event.Charcode            as Charcode
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A

data ETState = ETState
    { _theActions :: [ETAction]
    , _theText    :: !T.Text
    , _theBool    :: !Bool
    } deriving (Show)

data ETAction
    = ClearActionsA

    | KeyDownEnterA
    | KeyUpEnterA
    | KeyPressCapitalA
    | FocusA
    | BlurA
    | TextChangeA T.Text
    | BoolChangeA Bool
    | MouseEnterA Int
    | MouseLeaveA Int
    | MouseOverA Int
    | MouseOutA Int
    | MouseDownA
    | MouseUpA
    | ClickA
    | DoubleClickA
    | MouseMoveA Int Int
    | ScrollA Int
    deriving (Show, Typeable)

makeLenses ''ETState

applyETAction :: ETAction -> Transition ETState ETAction
applyETAction act = runTransitionM $ do
    theActions %= (act:)
    case act of
      ClearActionsA    -> theActions .= []
      TextChangeA text -> theText .= text
      BoolChangeA bool -> theBool .= bool
      _                   -> return ()

renderETState :: ETState -> WindowState ETAction
renderETState state = WindowState
    { _wsPath = ""
    , _wsBody = renderBody state
    }

renderBody :: ETState -> H.Html ETAction
renderBody (ETState actions text bool) = H.section $ do
    let textVal = A.value (H.toValue text)

    H.div H.! A.class_ "et-actions" $ do
      H.a H.! E.onClick' ClearActionsA $ "Clear event history"
      H.pre $ H.toHtml $ unlines $ map show actions

    H.h2 "Testing keyboard events"
    H.p "Type <enter> to test onKeyUp/onKeyDown"
    H.input
      H.! A.type_ "text"
      H.! E.onKeyDown [Keycode.enter] KeyDownEnterA
      H.! E.onKeyUp [Keycode.enter] KeyUpEnterA
    H.p "Type a capital letter to test onKeyPress"
    H.input
      H.! A.type_ "text"
      H.! E.onKeyPress (map Charcode.fromChar ['A'..'Z']) KeyPressCapitalA

    H.h2 "Testing focus events"
    H.p "Use the box below to test onBlur/onFocus"
    H.input
      H.! A.type_ "text"
      H.! E.onFocus FocusA
      H.! E.onBlur BlurA

    H.h2 "Testing form events"
    H.p "These elements share a text value. Use them to test onValueChange."
    H.p $ H.input    H.! textVal H.! E.onValueChange TextChangeA
    H.p $ H.textarea H.! textVal H.! E.onValueChange TextChangeA
    H.p $ H.select   H.! textVal H.! E.onValueChange TextChangeA $ do
      H.option H.! A.value "sheep" $ "Sheep"
      H.option H.! A.value "pig"   $ "Pig"
      H.option H.! A.value "cow"   $ "Cow"
    H.p "These elements share a boolean value. Use them to test onCheckedChange."
    H.p $ H.input H.! A.type_ "checkbox"
                  H.! A.checked bool
                  H.! E.onCheckedChange BoolChangeA
    H.p $ do
      H.input H.! A.type_ "radio"
              H.! A.checked bool
              H.! E.onCheckedChange BoolChangeA
      H.input H.! A.type_ "radio"
              H.! A.checked (not bool)
              H.! E.onCheckedChange (BoolChangeA . not)
    -- TODO (asayers): onSubmit

    H.h2 "Testing mouse events"
    H.p "Use the box below to test onMouseDown, onMouseUp, onClick, and onDoubleClick."
    H.div H.! A.class_ "et-box4"
          H.! E.onMouseDown'   MouseDownA
          H.! E.onMouseUp'     MouseUpA
          H.! E.onClick'       ClickA
          H.! E.onDoubleClick' DoubleClickA
          $ H.span "Click me!"
    H.p "Use the boxes below to test onMouseEnter, onMouseLeave, onMouseOver, and onMouseOut."
    H.div H.! A.class_ "et-box1"
          H.! E.onMouseOver  (const $ MouseOverA 1)
          H.! E.onMouseOut   (const $ MouseOutA 1)
          H.! E.onMouseEnter (const $ MouseEnterA 1)
          H.! E.onMouseLeave (const $ MouseLeaveA 1) $ do
      H.span "1"
      H.div H.! A.class_ "et-box2"
            H.! E.onMouseOver  (const $ MouseOverA 2)
            H.! E.onMouseOut   (const $ MouseOutA 2)
            H.! E.onMouseEnter (const $ MouseEnterA 2)
            H.! E.onMouseLeave (const $ MouseLeaveA 2) $ do
        H.span "2"
        H.div H.! A.class_ "et-box3"
              H.! E.onMouseOver  (const $ MouseOverA 2)
              H.! E.onMouseOut   (const $ MouseOutA 2)
              H.! E.onMouseEnter (const $ MouseEnterA 3)
              H.! E.onMouseLeave (const $ MouseLeaveA 3) $ "3"
    H.p "Use the box below to test onMouseMove"
    H.div H.! A.class_ "et-box4"
          H.! E.onMouseMove (\(E.MousePosition x y _ _ _ _ _ _) -> MouseMoveA x y)
          $ H.span "Move mouse here"

    H.h2 "Testing scroll events"
    H.p "Use the div below to test onScroll."
    H.div H.! A.class_ "et-box5"
          H.! E.onScroll ScrollA
          $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras at ligula orci. Nam efficitur ante justo, eget ultrices mauris maximus ut. Maecenas laoreet a lorem non fermentum. Vestibulum maximus porta quam, a pellentesque elit tristique eget. Sed sit amet lacus ut nunc malesuada aliquam. Sed leo erat, aliquam sagittis massa non, tempor condimentum ipsum. Nullam efficitur vel metus ac pharetra. Aenean in feugiat urna. Aenean in ante fermentum, sagittis ligula vel, malesuada arcu. Aliquam accumsan varius tempor. Etiam consectetur augue lorem, sit amet ullamcorper lacus finibus ac. Etiam semper euismod suscipit. Sed dapibus consectetur lacus, ac feugiat purus. Etiam lobortis ac leo et vehicula. Nullam pellentesque, lectus sed imperdiet viverra, odio felis rutrum sem, nec tristique ex enim eget velit. Phasellus semper venenatis metus, in sagittis mauris convallis et. Donec luctus ex a blandit sollicitudin. Vestibulum sollicitudin ipsum ut arcu ultrices molestie. Nulla id lorem tincidunt, ullamcorper libero ac, fringilla felis. Aenean rutrum et nisi eget commodo. Phasellus scelerisque ante eleifend elementum facilisis. Proin enim nisi, dapibus id nunc ac, dictum tincidunt neque. Cras ac viverra dolor, eu tincidunt nisl. Pellentesque finibus, massa pellentesque laoreet auctor, nisl odio pharetra velit, vel sollicitudin mi eros ac lectus."

    -- TODO (asayers): onWheel


app :: App ETState ETAction
app = App
    { appInitialState    = ETState [] "" True
    , appInitialRequests = []
    , appApplyAction     = applyETAction
    , appRender          = renderETState
    }
