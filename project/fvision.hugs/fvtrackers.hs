
-- This module contains tracker abstractions defined in the PADL01
-- FVision paper.  Part of the FVision code.

module FVTrackers where

import FRPSignal(delayB)  -- ??? why is this hidden?
import FRP
import Animate   -- reactimate & IO types
import XVTypes   -- types used in the underlying XV stuff
import FVTypes   -- Tracking abstractions
import NXVision2 -- Greencard stuff from XVision.
import FXVision2  -- stubs for functions not yet in NXVision2
import qualified GeometryStatic as S
import GeometryZ

-- Tracking abstractions from the PADL paper.

-- Join a observer and stepper to make a tracker.

mkTracker ::  Observer observation a -> Stepper measure observation a ->
              Tracker measure a
mkTracker o s = \(loc, image) -> let ob = o (loc, image) in s (loc, ob)

grabTransform2 :: Size -> Observer ImageRGB Transform2
grabTransform2 s (state, frame) =
  let (w,h) = sizeToISize s
      center = state S.%$ S.origin2
      rotation = snd $ S.vector2PolarCoords (state S.%$ (S.point2XY 1 0) S..-. center)
   in
      if abs rotation < pi / 1000 then 
         grabRectangle frame center w h
        else
         grabRotatedRectangle frame center w h rotation

ssdStep :: ImageRGB -> Stepper Residual ImageRGB Transform2
ssdStep reference (state, image) =
  let (translate, angle, r) = ssdTrack reference image
   in Residual {residual = r, resValue = S.translate2 translate `S.compose2` S.rotate2 angle}

ssdTracker :: ImageRGB -> Tracker Residual Transform2
ssdTracker image =
 mkTracker (grabTransform2 (sizeOfImage image)) (ssdStep image)

runTracker :: Valued measure =>
   VideoB -> a -> Tracker measure a -> VBehavior a
runTracker video a0 tracker = locations where
 locations  = delayB a0 aStream
 ma         = lift1 tracker (lift2 (,) locations video)
 aStream    = lift1 valueOf ma

bestOf :: (Functor measure, Ord (measure (a, Bool))) => 
  Tracker measure a -> Tracker measure a -> Tracker measure (a, Bool)
bestOf t1 t2 =
 \((loc, _), v) -> max (fmap (\x -> (x, True))  (t1 (loc, v)))
                       (fmap (\x -> (x, False)) (t2 (loc, v)))

type Predictor a = VBehavior (Time -> a)

runTrackerPred :: Valued measure =>
   VideoB -> Tracker measure a -> Predictor a -> VBehavior a
runTrackerPred video tracker predictor = 
    lift2 (\p v -> valueOf (tracker (p, v))) (predictor $* timeB) video

interp2 :: Point2 -> VBehavior Point2 -> Predictor Point2
interp2 p0 b = lift4 interp back1 back2 t1 t2
  where back1 = delayB p0 b
        back2 = delayB p0 back1
        t1 = delayB 0 timeB
        t2 = delayB 0 t1
        interp p1 p2 _ 0 = const p2
        interp p1 p2 t1 t2 =
          let dt = t2 - t1
              v  = p2 S..-. p1 in
           \t -> p1 S..+^ (((t - t1) / (t2 - t1)) S.*^ v)
          
{-
type EPair a b = (a -> b, b -> a)

edgeStepper :: Stepper Sharpness Image LineSeg
edgeStepper (lineSeg, image) = trackEdge image 

type Stepper measure observation a =
    (a, observation) -> measure a
-}
