
-- Converted to new frp -- jcp

-- These used to be in the Geometry module.  

-- OLD CODE    DOES NOT WORK!!!

module FVUtils where

import FRP
import GeometryStatic
import XVTypes

-- Support for integer points (used in XVision functions)

iPointToPoint2 :: IPoint -> Point2
iPointToPoint2 (x, y) = point2XY (fromInt x) (fromInt y)


point2ToIPoint :: Point2 -> IPoint
point2ToIPoint (Point2XY x y) =  (round x, round y)


-- Geometry stuff that isn't yet in the FRP geometry module

end1, end2 :: LineSegment2 -> Point2

end1 (LineSegment2 p v) = p
end2 (LineSegment2 p v) = p .+^ v

lineSegment2V :: LineSegment2 -> Vector2
lineSegment2V (LineSegment2 _ v) = v

midpt :: LineSegment2 -> Point2
midpt (LineSegment2 p v) = p .+^ 0.5 *^ v

mkSegment2PV :: Point2 -> Vector2 -> LineSegment2
mkSegment2PV = LineSegment2

mkSegment2PP :: Point2 -> Point2 -> LineSegment2
mkSegment2PP p1 p2 = LineSegment2 p1 (p2 .-. p1)

-- Line segment defines height, width passed directly

segmentToRegion :: LineSegment2 -> Scalar -> Region
segmentToRegion l@(LineSegment2 t v) w = 
  Region (midpt l) (Size w h) (a+pi/2) -- I think this should be (Size h w) a
 where
  (h,a) = vector2PolarCoords v

lengthSegment2 :: LineSegment2 -> Scalar
lengthSegment2 (LineSegment2 _ v) = magnitude v

-- The math in this function really needs to be cleaned up!  I don't
-- understand exactly what is going on.  -- jcp
-- This code took a lot of experiment ot get it to "work".
-- The problem is that I describe the region I want to grab
-- in terms that are very different from the XVision paper
-- (and implementation).  In particular, the "angle" of an
-- edge is 90 degrees different from what the paper describes
-- (hence the pi/2 in segmentToRegion).
addDeltaSegment :: LineSegment2 -> Length -> Radians -> LineSegment2
addDeltaSegment (LineSegment2 p v) dt da = LineSegment2 newP newV
 where
  (l,a) = vector2PolarCoords v
  a' = a + da   -- New angle
  deltaP = vector2Polar dt' a
--  newP = point2XY (x-dt'*s) (y-dt'*c) -- p - vector2Polar a dt (?)
  newP = p .-^ deltaP
  newV = vector2Polar l a'  -- length is unchanged

  -- This correction is based on code in Line.cc
  -- ToDo: should take sampling ratio into account
--  dt' = dt*(cos da) + dt*(sin da);
  dt' = dt*(cos da + sin da) -- Eh??  I don't grok this!! jcp
  

--intersection :: LineSegment -> LineSegment -> Pt
--intersection (p1,v1) (p2,v2) 
--  | det == 0 = p1 -- randomly choose either point if colinear
--		    -- midpoint might be more robust?
--  | otherwise = p1 `addV` (s1 `vscale` v1)
-- where
--  det = x2*y1 - x1*y2 
--
--  s2 = (dx*y1 - dy*x1) / det
--  s1 = (dy*x2 - dx*y2) / det
--
--  (dx,dy) = asCartesian (p1 `subPt` p2)
--
--  (x1,y1) = asCartesian v1
--  (x2,y2) = asCartesian v2

intersection :: LineSegment2 -> LineSegment2 -> Point2
intersection (LineSegment2 p1 v1) (LineSegment2 p2 v2) =
    p1 .+^ (s1 *^ v1)
 where
  (dx,dy) = vector2XYCoords (p1 .-. p2)
  (x1,y1) = vector2XYCoords v1
  (x2,y2) = vector2XYCoords v2
  (s2,s1) = (a*dx+b*dy, c*dx+d*dy)
  (a,b,c,d) = inverse (x2,y2,-x1,-y1)

-- invert a 2x2 matrix 
-- Oops! Blow up when the determinate is 0!

inverse :: (RealVal,RealVal,RealVal,RealVal) -> (RealVal,RealVal,RealVal,RealVal)
inverse (a,b,c,d) = (d/det, -c/det, -b/det, a/det)
 where
  det = a*d - b*c


iSize :: Int -> Int -> Size
iSize x y = Size (fromInt x) (fromInt y)

toSize :: ISize -> Size
toSize (x,y) = iSize x y
sizeWidth (Size x y) = x
sizeHeight (Size x y) = y

toISize (Size x y) = (round x, round y) 

showSized :: Sized a => String -> a -> ShowS
showSized nm x = showString "<<" . showString nm .
                 showString " " .
                 shows w . showString " x " .
                 shows h . showString " >>"
  where
    (w,h) = toISize $ sizeOf x  
 
transposeSize (Size x y) = Size y x

instance Num Size where
  Size x y + Size a b = Size (x+a) (y+b)
  Size x y - Size a b = Size (x-a) (y-b)  --?? check for positive sizes?
  fromInteger i = Size (fromInteger i) (fromInteger i)
  negate (Size x y) = Size (-x) (-y)   ---????

-- Should be more general

areaOf :: Size -> RealVal
areaOf (Size x y) = x*y

