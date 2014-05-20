-- Danger!  Danger!  I haven't finished converting this file to the new system!
-- type errors abound!!  jcp

----------------------------------------------------------------
-- Stream Processors
--
-- Choice of symbols is inspired by John Hughes' Arrow combinators
----------------------------------------------------------------

module SPtalk
       ( module SPtalk
       ) where

import XVTypes
import Geometry
import Draw
import SP hiding(composite2)
import Pipe
--import SSD hiding(interactive_init,Delta,ssd)
import Matrix
import Image
import Video
import Window
import SSD
import Init

import IOExts
import Monad
import Maybe
import XVUtilities
import List( transpose )
import Array

-- Basic types used:

type Delta a = (a -> a)
-- Should just have one type here!
type AugmentedState st = WithError st
type AugmentedDelta st = WithError (Delta st)
type Pos = Transform2   -- Defines a transformation onto the sampling area

--- Alastair's stream processor definition

-- type SP a b = Pipe a -> Pipe b

--- Breaking tracking apart into acquisition and tracking;
--- note that the augmented state stuff is really screwy; the
--- error should be part of the state

type Src clk st  = SP clk st ImageInt
type Stepper clk st = SP clk ImageInt (AugmentedDelta st)
-- type Tracker st = SP State  (AugmentedState st)

--- Something to generate a tracker from components
--- The definition of state integral seems a bit of a hack
--- for has to be this way to carry error along.  Most likely
--- we should just include the error in the state all the way
--- around the loop.


--- Let's just get this out of the way; consoles are going
--- to go away; for the purposes of this file we'll declare
--- one and keep it around; in general this could done in the prolog
--- once and never done again.

c = unsafePerformIO (openConsole True True)

--- If you just create a window, it resizes to fit whatever is
--- display (of course, you get no name...) and setting the
--- name when it opens croaks for some reason

--newWindow nm = createnamedWindow c nm
newWindow nm = createWindow c

--- Otherwise, open it with a size

--- Some other IO stuff

newsrc :: Video -> Size -> Int -> Src Pos
newsrc vidsrc sz res =
    (newsrcC vidsrc sz res) >>> (sp1 colorToBWImage)

newsrcC vidsrc sz res = 
    spIO1 (\pos -> acquireImage NoOptions vidsrc sz
                     (iscale2 res `compose2` pos))


stateIntegral:: st -> SP (AugmentedDelta st) (AugmentedState st)
stateIntegral start = 
         \x -> join2 (integral start ((lift1 fst) x), (lift1 snd) x)

basicTracker:: Stepper a -> Src a -> a -> Tracker a
basicTracker step src start =
     src >>> step >>> (stateIntegral start)

---- Some other ways of making stream processor with side effects ------

makeIOSink fn = spIO1 fn
makeIOSideEffect fn = spIO1 (\x -> do {fn; return x})

makeIOSP:: (a->IO ()) -> SP a a
makeIOSP fn = spIO1 (\x -> do {fn x; return x})

displaySP w = makeIOSP (\x -> (showImage w (bwToColorImage x)))
displaySink w = makeIOSink (\x -> showImage w (bwToColorImage x))

diskSink w = makeIOSink (\x -> do {clearWindow w 0;drawDisk w x;flushWindow w})
intersectionSink w = makeIOSink (drawIntersection w)
clearSP w = makeIOSideEffect (clearWindow w 0)

--- Ways of making ssd trackers

ssd :: ImageInt -> Src Transform2 -> Transform2 -> Tracker Transform2
ssd im src pos = basicTracker (lift1 (ssdStep im)) src pos

interactiveSSD :: Video -> (Int,Int) -> Int -> IO ((Tracker Pos),Pos)
interactiveSSD v sz res = do
    Point2XY x y <- interactiveInitPoint2 v 
    let state = (fromInt x, fromInt y, 0.0)
    im <- grab v (fromInt res) sz state
    return (ssd im (newsrc v sz res) state , state)




--- We need a little better calculus of building these things
--- up I guess; I don't think is it ....

x `branch` y = (x &&& (x >>> y)) >>> (sp1 fst)

myt sz = 
   do 
     { (vid,_) <- openVideo myvideoDevice
     ; (ssd,s) <- interactiveSSD vid (sz,sz) 3
     ; w <- newWindow "Where"
     ; let showstate = (lift1 fst) >>>             --- Take the state
                       (newsrc vid (sz,sz) 1) >>>  --- grab the image there
                       displaySink w               --- and display t
     ; let disp = (\x -> do {forwardVideo vid;print x})
     ; let pipeline = ssd `branch` showstate
     ; runSP disp s pipeline
      }

myseg sz = 
   do 
     { (vid,_) <- openVideo myvideoDevice
     ; (ssd,s) <- interactiveSeg vid (sz,sz) 3
     ; w <- newWindow "Where"
--     ; let showstate = (lift1 fst) >>>             --- Take the state
--                       (newsrc vid (sz,sz) 1) >>>  --- grab the image there
--                       displaySink w               --- and display t
     ; let disp = (\x -> do {forwardVideo vid;print x})
     ; let pipeline = ssd -- `branch` showstate
     ; runSP disp s pipeline
      }

todisk rad (x,y,th) = ((x,y),rad)
tointersection rad (x,y,th) = 
              (pt,v1,v2) where
                  pt = (x,y)
                  v1 = Cartesian 0 rad
                  v2 = Cartesian rad 0 

mydraw :: Window -> Integer -> IO ()
mydraw w n | n > 0 = do
  { clearWindow w 0
  ; drawDisk w ((50+ (fromInteger n),50+(fromInteger n)), 20)
  ; mydraw w (n - 1)
  }

mydraw w n = return ()

drawit = 
   do 
     { (vid,_) <- openVideo myvideoDevice
     ; w <- newsizedWindow "Where" (size vid)
     ; mydraw w 300 
      }

follow sz = 
   do 
     { (vid,_) <- openVideo myvideoDevice
     ; (ssd,s) <- interactiveSSD vid sz 3
     ; w <- newsizedWindow "Where" (size vid)
     ; clearWindow w 0
     ; let showstate = (lift1 fst) >>>           --- Take the state
                       (sp1 (todisk 20)) >>>     --- convert to  disks
                       (diskSink w)              --- and display t
     ; let disp = (\x -> do {forwardVideo vid;print x})
     ; let pipeline = ssd `branch` showstate
     ; runSP disp s pipeline
      }

--- A pair with the property (f,g) such that (g f) a = a  (g is a left
--- inverse of a)

type Splitter a a1 a2 = a -> (a1,a2)
type Joiner   a1 a2 a = (AugmentedState a1, AugmentedState a2) -> AugmentedState a
type EPair a1 a2 a = (Splitter a a1 a2, Joiner a1 a2 a)

composite2 :: 
           EPair a1 a2 a
           -> Tracker a1
           -> Tracker a2
           -> Tracker a
composite2 (split,join) t1 t2 = (sp1 split) >>> (t1 *** t2) >>> sp1 join

-- An example of the above

winnersp :: Splitter a a a
winnersp a = (a, a)

winnerjn :: Joiner a a a
winnerjn ((a1, err1), (a2, err2)) = if (err1 < err2) then (a1,err1) else (a2,err2)

bestof :: Tracker a -> Tracker a -> Tracker a
bestof f g = composite2 (winnersp, winnerjn) f g

--- Specialization of best to SSD's; note this should be factored
--- further back to avoid grabbing two images

type Best a    = (a,a)  -- a good one and a bad one
type Comp2 a b = (a,b)  -- a left one and a right one

bestSSD :: Best ImageInt -> Src Pos -> Pos -> Tracker Pos
bestSSD (i1, i2) src p0 = bestof (ssd i1 src p0) (ssd i2 src p0)

{-
trackMouth v = bestSSD mouthIms (newsrcI v sizeof mouthIms)
trackLEye v  = bestSSD leyeIms (newsrcI v sizeof leyeIms)
trackREye v  = bestSSD reyeIms (newsrcI v sizeof reyeIms)
-}

pttoSegsp :: Splitter LineSegment2 Pos Pos
pttoSegsp seg = ((x1,y1,a),(x2,y2,a))
       where
         (pt,v) = seg
         (x1,y1) = end1 seg
         (x2,y2) = end2 seg
         (_,a) = asPolar v


pttoSegjn :: Joiner Pos Pos LineSegment2
pttoSegjn (((x1,y1,_), err1), ((x2,y2,_), err2)) = 
        (mkSeg2 (x1,y1)  (x2,y2), (max err1 err2))


pttoSeg :: Tracker Pos -> Tracker Pos -> Tracker LineSegment2
pttoSeg f g = composite2 (pttoSegsp, pttoSegjn) f g

interactiveSeg v sz res = 
  do { (ssd1,s1) <- (interactiveSSD v sz res)
     ; (ssd2,s2) <- (interactiveSSD v sz res)
     ; let t =  pttoSeg ssd1 ssd2
     ; return (t,fst (pttoSegjn ((s1,0),(s2,0))))
     }

-- trackClown v = composite2 identity (trackEyes v) (trackMouth v)
--   where
--      split = ....
--      join  = ....



--runSP :: (a -> IO ()) -> a -> Tracker a -> IO ()
--runSP display init t = runPipe (lift1 (display.fst) (loopSP init fst t))

--loopSP :: a -> (b -> a) -> SP a b -> Pipe b
--loopSP a0 f sp = let b = sp (delay a0 a)
--                     a = lift1 f b
--                 in b

