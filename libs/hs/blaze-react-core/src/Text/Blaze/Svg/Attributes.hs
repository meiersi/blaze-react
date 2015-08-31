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
cx = attribute "cx" " cx=\""

cy :: AttributeValue -> Attribute ev
cy = attribute "cy" " cy=\""

d :: AttributeValue -> Attribute ev
d = attribute "d" " d=\""

dx :: AttributeValue -> Attribute ev
dx = attribute "dx" " dx=\""

dy :: AttributeValue -> Attribute ev
dy = attribute "dy" " dy=\""

fill :: AttributeValue -> Attribute ev
fill = attribute "fill" " fill=\""

fillOpacity :: AttributeValue -> Attribute ev
fillOpacity = attribute "fillOpacity" " fillOpacity=\""

fontFamily :: AttributeValue -> Attribute ev
fontFamily = attribute "fontFamily" " fontFamily=\""

fontSize :: AttributeValue -> Attribute ev
fontSize = attribute "fontSize" " fontSize=\""

fx :: AttributeValue -> Attribute ev
fx = attribute "fx" " fx=\""

fy :: AttributeValue -> Attribute ev
fy = attribute "fy" " fy=\""

gradientTransform :: AttributeValue -> Attribute ev
gradientTransform = attribute "gradientTransform" " gradientTransform=\""

gradientUnits :: AttributeValue -> Attribute ev
gradientUnits = attribute "gradientUnits" " gradientUnits=\""

markerEnd :: AttributeValue -> Attribute ev
markerEnd = attribute "markerEnd" " markerEnd=\""

markerMid :: AttributeValue -> Attribute ev
markerMid = attribute "markerMid" " markerMid=\""

markerStart :: AttributeValue -> Attribute ev
markerStart = attribute "markerStart" " markerStart=\""

offset :: AttributeValue -> Attribute ev
offset = attribute "offset" " offset=\""

opacity :: AttributeValue -> Attribute ev
opacity = attribute "opacity" " opacity=\""

patternContentUnits :: AttributeValue -> Attribute ev
patternContentUnits = attribute "patternContentUnits" " patternContentUnits=\""

patternUnits :: AttributeValue -> Attribute ev
patternUnits = attribute "patternUnits" " patternUnits=\""

points :: AttributeValue -> Attribute ev
points = attribute "points" " points=\""

preserveAspectRatio :: AttributeValue -> Attribute ev
preserveAspectRatio = attribute "preserveAspectRatio" " preserveAspectRatio=\""

r :: AttributeValue -> Attribute ev
r = attribute "r" " r=\""

rx :: AttributeValue -> Attribute ev
rx = attribute "rx" " rx=\""

ry :: AttributeValue -> Attribute ev
ry = attribute "ry" " ry=\""

spreadMethod :: AttributeValue -> Attribute ev
spreadMethod = attribute "spreadMethod" " spreadMethod=\""

stopColor :: AttributeValue -> Attribute ev
stopColor = attribute "stopColor" " stopColor=\""

stopOpacity :: AttributeValue -> Attribute ev
stopOpacity = attribute "stopOpacity" " stopOpacity=\""

stroke :: AttributeValue -> Attribute ev
stroke = attribute "stroke" " stroke=\""

strokeDasharray :: AttributeValue -> Attribute ev
strokeDasharray = attribute "strokeDasharray" " strokeDasharray=\""

strokeLinecap :: AttributeValue -> Attribute ev
strokeLinecap = attribute "strokeLinecap" " strokeLinecap=\""

strokeOpacity :: AttributeValue -> Attribute ev
strokeOpacity = attribute "strokeOpacity" " strokeOpacity=\""

strokeWidth :: AttributeValue -> Attribute ev
strokeWidth = attribute "strokeWidth" " strokeWidth=\""

textAnchor :: AttributeValue -> Attribute ev
textAnchor = attribute "textAnchor" " textAnchor=\""

transform :: AttributeValue -> Attribute ev
transform = attribute "transform" " transform=\""

version :: AttributeValue -> Attribute ev
version = attribute "version" " version=\""

viewBox :: AttributeValue -> Attribute ev
viewBox = attribute "viewBox" " viewBox=\""

x1 :: AttributeValue -> Attribute ev
x1 = attribute "x1" " x1=\""

x2 :: AttributeValue -> Attribute ev
x2 = attribute "x2" " x2=\""

x :: AttributeValue -> Attribute ev
x = attribute "x" " x=\""

y1 :: AttributeValue -> Attribute ev
y1 = attribute "y1" " y1=\""

y2 :: AttributeValue -> Attribute ev
y2 = attribute "y2" " y2=\""

y :: AttributeValue -> Attribute ev
y = attribute "y" " y=\""
