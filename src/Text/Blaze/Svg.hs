{-# LANGUAGE OverloadedStrings #-}

-- | This module exorts SVG combinators supported in react.
module Text.Blaze.Svg
    ( module Text.Blaze
    , Svg
    , toSvg
    , preEscapedToSvg

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

preEscapedToSvg :: ToMarkup a => a -> Svg ev
preEscapedToSvg = preEscapedToMarkup

circle :: Svg ev
circle = Leaf "circle" "<circle" ">"

defs :: Svg ev -> Svg ev
defs = Parent "defs" "<defs" "</defs>"

ellipse :: Svg ev
ellipse = Leaf "ellipse" "<ellipse" ">"

g :: Svg ev -> Svg ev
g = Parent "g" "<g" "</g>"

line :: Svg ev
line = Leaf "line" "<line" ">"

linearGradient :: Svg ev -> Svg ev
linearGradient = Parent "linearGradient" "<linearGradient" "</linearGradient>"

mask :: Svg ev -> Svg ev
mask = Parent "mask" "<mask" "</mask>"

path :: Svg ev
path = Leaf "path" "<path" ">"

pattern :: Svg ev -> Svg ev
pattern = Parent "pattern" "<pattern" "</pattern>"

polygon :: Svg ev
polygon = Leaf "polygon" "<polygon" ">"

polyline :: Svg ev
polyline = Leaf "polyline" "<polyline" ">"

radialGradient :: Svg ev -> Svg ev
radialGradient = Parent "radialGradient" "<radialGradient" "</radialGradient>"

rect :: Svg ev
rect = Leaf "rect" "<rect" ">"

stop :: Svg ev
stop = Leaf "stop" "<stop" ">"

text :: Svg ev -> Svg ev
text = Parent "text" "<text" "</text>"

tspan :: Svg ev -> Svg ev
tspan = Parent "tspan" "<tspan" "</tspan>"
