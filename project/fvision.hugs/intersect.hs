
-- OLD CODE: NOT TESTED

----------------------------------------------------------------
-- Intersection tracker
----------------------------------------------------------------

module Intersect
	( intersect
	, corner, tee, cross	
	) where

import SP
	( Tracker, composite2
	, spTrace
	)
import Geometry
import XVTypes

----------------------------------------------------------------
-- Exports
----------------------------------------------------------------

intersect :: (Scalar, Scalar) -> Tracker LineSegment2 ->
             Tracker LineSegment2 -> Tracker Intersection

corner :: Tracker LineSegment2 -> Tracker LineSegment2 -> Tracker Intersection
tee    :: Tracker LineSegment2 -> Tracker LineSegment2 -> Tracker Intersection
cross  :: Tracker LineSegment2 -> Tracker LineSegment2 -> Tracker Intersection

----------------------------------------------------------------
-- Implementations
----------------------------------------------------------------

intersect (s1,s2) t1 t2 = composite2 split join t1 t2
 where
  split :: Intersection -> (LineSegment2, LineSegment2)
  split (pt,v1,v2) = (mkSegment2PV pt1 v1, mkSegment2PV pt2 v2)
   where
    pt1 = pt .+^ s1 *^ v1
    pt2 = pt .+^ s2 *^ v2

  join :: LineSegment2 -> LineSegment2 -> Intersection
  join l1 l2 = (intersection l1 l2, lineSegment2V l1, lineSegment2V l2)

corner = intersect (0,0)
tee    = intersect (0.5,1)
cross  = intersect (0.5,0.5)

----------------------------------------------------------------
-- End
----------------------------------------------------------------