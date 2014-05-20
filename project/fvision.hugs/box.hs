----------------------------------------------------------------
-- Box (4 sided rigid body) tracker
--
-- This is intended as a demo of what the new trackers can do
-- rather than being a fully-fledged reusable component

-- Not yet adapted for the new FVision
----------------------------------------------------------------

module Box
	( box
	) where

import XVTypes
import Intersect
	( corner
	)
import Video

import SP
	( Tracker, composite4
	, spTrace
	)
import Geometry

import Edge
	( tracker
	)

import IOExts

----------------------------------------------------------------
-- Exports
----------------------------------------------------------------


-- The scalar specifies the length of the edges on each corner
box :: Video -> Size -> Tracker Box

----------------------------------------------------------------
-- Implementations
----------------------------------------------------------------

-- ADR's original cut at this used integral sizes

box v (Size w h) = composite4 split join corn corn corn corn
 where
  split :: Box -> (Intersection,Intersection,Intersection,Intersection)
  split (a,b,c,d) = (vertex d a b, vertex a b c, vertex b c d, vertex c d a)

  join :: Intersection -> Intersection -> Intersection -> Intersection -> Box
  join idab iabc ibcd icda =
     (intersectionPoint idab,
      intersectionPoint iabc,
      intersectionPoint ibcd,
      intersectionPoint icda)

  vertex :: Point2 -> Point2 -> Point2 -> Intersection
  vertex x y z = unsafePerformIO $
    do putStr "In Vertex\n"
       putStr "x="
       print x
       putStr "y="
       print y
       putStr "z="
       print z
       putStr "Intersection = "
       print (mkIntersection y v1 v2)
       return (mkIntersection y v1 v2)
   where
    v1 = ((x .-. y) `scaledTo` h)
    v2 = ((z .-. y) `scaledTo` h)

  corn :: Tracker Intersection
  corn = corner (Edge.tracker v w') (Edge.tracker v w')

  w' = round w

----------------------------------------------------------------
-- End
----------------------------------------------------------------


