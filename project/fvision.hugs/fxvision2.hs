
-- This is a dummy module for debugging purposes.

module FXVision2 where

import qualified GraphicsEvent as G
import GeometryZ
import XVBaseTypes
import NXVision2

-- Stubs

data FVColor = FVColor

sizeOfImage :: ImageRGB -> Size
sizeOfImage = undefined

-- Grabbers for subimages.  Should allocate a new buffer ...

grabRectangle :: ImageRGB {- the current frame -} -> 
                 Point2 {- center -} ->
                 Int {- width -} -> 
                 Int {- height -} -> 
                 ImageRGB
grabRectangle = undefined

grabRotatedRectangle :: ImageRGB {- the current frame -} -> 
                        Point2 {- center -} ->
                        Int {- width -} -> 
                        Int {- height -} -> 
                        FRPReal {- angle in radians -} ->
                        ImageRGB
grabRotatedRectangle = undefined

grabColor :: ImageRGB -> Point2 -> FVColor
grabColor = undefined

trackBlob :: ImageRGB {- sampled sub-image -} ->
             FVColor {- really a set of colors -} ->
             (Point2, Int, Int, Int) {- Center of blob, number of
                                          pixels, height, width -}
trackBlob = undefined

trackEdge :: ImageRGB -> (Vector2 {- displacement of region -},
                          FRPReal, {- angle -}
                          FRPReal) {- sharpness -}
trackEdge = undefined

ssdTrack :: ImageRGB {- Reference -} -> ImageRGB {- Current -} ->
            (Vector2, FRPReal {- Angle -}, FRPReal {- Residual -})
ssdTrack = undefined
