-- | This module exports combinators that provide you with the
-- ability to set attributes on SVG elements.
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Svg.Attributes
    ( cx
    , cy
    , d
    , dx
    , dy
    , fill
    , fillOpacity
    , fontFamily
    , fontSize
    , fx
    , fy
    , gradientTransform
    , gradientUnits
    , markerEnd
    , markerMid
    , markerStart
    , offset
    , opacity
    , patternContentUnits
    , patternUnits
    , points
    , preserveAspectRatio
    , r
    , rx
    , ry
    , spreadMethod
    , stopColor
    , stopOpacity
    , stroke
    , strokeDasharray
    , strokeLinecap
    , strokeOpacity
    , strokeWidth
    , textAnchor
    , transform
    , version
    , viewBox
    , x1
    , x2
    , x
    , y1
    , y2
    , y
    ) where

import Text.Blaze.Internal (Attribute, AttributeValue, attribute)

cx :: AttributeValue -> Attribute ev
cx = attribute "cx"

cy :: AttributeValue -> Attribute ev
cy = attribute "cy"

d :: AttributeValue -> Attribute ev
d = attribute "d"

dx :: AttributeValue -> Attribute ev
dx = attribute "dx"

dy :: AttributeValue -> Attribute ev
dy = attribute "dy"

fill :: AttributeValue -> Attribute ev
fill = attribute "fill"

fillOpacity :: AttributeValue -> Attribute ev
fillOpacity = attribute "fillOpacity"

fontFamily :: AttributeValue -> Attribute ev
fontFamily = attribute "fontFamily"

fontSize :: AttributeValue -> Attribute ev
fontSize = attribute "fontSize"

fx :: AttributeValue -> Attribute ev
fx = attribute "fx"

fy :: AttributeValue -> Attribute ev
fy = attribute "fy"

gradientTransform :: AttributeValue -> Attribute ev
gradientTransform = attribute "gradientTransform"

gradientUnits :: AttributeValue -> Attribute ev
gradientUnits = attribute "gradientUnits"

markerEnd :: AttributeValue -> Attribute ev
markerEnd = attribute "markerEnd"

markerMid :: AttributeValue -> Attribute ev
markerMid = attribute "markerMid"

markerStart :: AttributeValue -> Attribute ev
markerStart = attribute "markerStart"

offset :: AttributeValue -> Attribute ev
offset = attribute "offset"

opacity :: AttributeValue -> Attribute ev
opacity = attribute "opacity"

patternContentUnits :: AttributeValue -> Attribute ev
patternContentUnits = attribute "patternContentUnits"

patternUnits :: AttributeValue -> Attribute ev
patternUnits = attribute "patternUnits"

points :: AttributeValue -> Attribute ev
points = attribute "points"

preserveAspectRatio :: AttributeValue -> Attribute ev
preserveAspectRatio = attribute "preserveAspectRatio"

r :: AttributeValue -> Attribute ev
r = attribute "r"

rx :: AttributeValue -> Attribute ev
rx = attribute "rx"

ry :: AttributeValue -> Attribute ev
ry = attribute "ry"

spreadMethod :: AttributeValue -> Attribute ev
spreadMethod = attribute "spreadMethod"

stopColor :: AttributeValue -> Attribute ev
stopColor = attribute "stopColor"

stopOpacity :: AttributeValue -> Attribute ev
stopOpacity = attribute "stopOpacity"

stroke :: AttributeValue -> Attribute ev
stroke = attribute "stroke"

strokeDasharray :: AttributeValue -> Attribute ev
strokeDasharray = attribute "strokeDasharray"

strokeLinecap :: AttributeValue -> Attribute ev
strokeLinecap = attribute "strokeLinecap"

strokeOpacity :: AttributeValue -> Attribute ev
strokeOpacity = attribute "strokeOpacity"

strokeWidth :: AttributeValue -> Attribute ev
strokeWidth = attribute "strokeWidth"

textAnchor :: AttributeValue -> Attribute ev
textAnchor = attribute "textAnchor"

transform :: AttributeValue -> Attribute ev
transform = attribute "transform"

version :: AttributeValue -> Attribute ev
version = attribute "version"

viewBox :: AttributeValue -> Attribute ev
viewBox = attribute "viewBox"

x1 :: AttributeValue -> Attribute ev
x1 = attribute "x1"

x2 :: AttributeValue -> Attribute ev
x2 = attribute "x2"

x :: AttributeValue -> Attribute ev
x = attribute "x"

y1 :: AttributeValue -> Attribute ev
y1 = attribute "y1"

y2 :: AttributeValue -> Attribute ev
y2 = attribute "y2"

y :: AttributeValue -> Attribute ev
y = attribute "y"
