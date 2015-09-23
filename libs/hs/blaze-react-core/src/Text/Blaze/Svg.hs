{-# LANGUAGE OverloadedStrings #-}

-- | This module exorts SVG combinators supported in react.
module Text.Blaze.Svg
    ( module Text.Blaze
    , Svg
    , toSvg

    , circle
    , defs
    , ellipse
    , g
    , line
    , linearGradient
    , mask
    , path
    , pattern
    , polygon
    , polyline
    , radialGradient
    , rect
    , stop
    , text
    , tspan
    ) where

import Text.Blaze
import Text.Blaze.Internal hiding (text)

type Svg ev = Markup ev

toSvg :: ToMarkup a => a -> Svg ev
toSvg = toMarkup

circle :: Svg ev
circle = Leaf "circle" False

defs :: Svg ev -> Svg ev
defs = Parent "defs"

ellipse :: Svg ev
ellipse = Leaf "ellipse" False

g :: Svg ev -> Svg ev
g = Parent "g"

line :: Svg ev
line = Leaf "line" False

linearGradient :: Svg ev -> Svg ev
linearGradient = Parent "linearGradient"

mask :: Svg ev -> Svg ev
mask = Parent "mask"

path :: Svg ev
path = Leaf "path" False

pattern :: Svg ev -> Svg ev
pattern = Parent "pattern"

polygon :: Svg ev
polygon = Leaf "polygon" False

polyline :: Svg ev
polyline = Leaf "polyline" False

radialGradient :: Svg ev -> Svg ev
radialGradient = Parent "radialGradient"

rect :: Svg ev
rect = Leaf "rect" False

stop :: Svg ev
stop = Leaf "stop" False

text :: Svg ev -> Svg ev
text = Parent "text"

tspan :: Svg ev -> Svg ev
tspan = Parent "tspan"
