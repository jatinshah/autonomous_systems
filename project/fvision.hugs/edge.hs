----------------------------------------------------------------
-- Edge tracker
----------------------------------------------------------------
-- OLD CODE    DOES NOT WORK!!!


module Edge
	( tracker
	) where

import XVTypes
import Video
import XVision as XV
	( mkEdgeTracker, find_EdgeTracker
	, xpad_EdgeTracker, ypad_EdgeTracker
	)
import SP
	( Tracker, statefulSP
	)
import Error

import Geometry

----------------------------------------------------------------
-- Exports
-- 
-- This is not the most general version: we ought to be able to change
-- the length without changing the width
-- also ought to be able to change scaling
----------------------------------------------------------------

-- Not the same as the one in XVision.gc!

data EdgeTracker = EdgeTracker XV.EdgeTracker Int

instance Show EdgeTracker where
  showsPrec _ (EdgeTracker et i) =
      showString "Edge Tracker"

make    :: Int -> IO EdgeTracker
update  :: Video -> EdgeTracker -> LineSegment2 -> IO (WithError LineSegment2)
tracker :: Video -> Int -> Tracker LineSegment2

----------------------------------------------------------------
-- Implementations
----------------------------------------------------------------

maskWidth :: Int
maskWidth = 8

make width = do
   et <- mkEdgeTracker maskWidth 0
   return (EdgeTracker et width)

-- ToDo: apparently a value of 0 means the line was not found.
-- ToDo: this is the exact inverse of what I thought it meant.

-- ToDo: not sure if I'm using the padding quite right since
-- it only gets applied to (w,h) instead of being applied
-- to the segment length as well.

-- Ought to redo this whole thing with transforms.

update v (EdgeTracker et w) line = do
   putStrLn ("Edge tracking " ++ show line)
   putStrLn ("Area: " ++ show (toSize (w+xpad,h+ypad)))
   putStrLn ("Region: " ++ show (segmentToRegion line (fromInt (w+xpad))))
   pic   <- acquireRegion v (toSize (w+xpad,h+ypad))
                 (segmentToRegion line (fromInt (w+xpad)))
   (x,angle,err) <- find_EdgeTracker et pic

--   print line
--   print (segmentToRegion line (fromInt w))
   let line' = addDeltaSegment line x angle
   putStr ("Line from " ++ show (end1 line) ++ " to " ++ show (end2 line) 
          ++ "\n")
   putStrLn ("delta = " ++ show (x,angle,err))
   putStr ("Updated to " ++ show (end1 line') ++ " to " ++ show (end2 line') 
          ++ "\n")
   return (WithError line' err)
 where
  h = round (lengthSegment2 line)
  xpad = xpad_EdgeTracker et (w,h)
  ypad = ypad_EdgeTracker et (w,h)

tracker v w = statefulSP (make w) (update v)

----------------------------------------------------------------
-- End
----------------------------------------------------------------

