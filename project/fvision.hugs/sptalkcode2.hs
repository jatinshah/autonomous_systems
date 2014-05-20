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
import SP hiding(composite2,Tracker)
-- import Pipe
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
import CStream
import XVision(clrproject)
import FranCore(scanlE)
import FVision(createMPEGSourceDisplay)


-- Basic types used:

-- type Delta a = (a -> a)
--type AugmentedDelta st = WithError (Delta st)
type AugmentedState st = WithError st
type AugmentedDelta st = WithError st

-->>-->> IP: Commented Delta type does not work with what I have written.

type Pos = Transform2   -- Defines a transformation onto the sampling area

type Src clk st  = SP clk st ImageInt
type Stepper clk st = SP clk ImageInt (AugmentedDelta st)
type Tracker clk st = SP clk st (AugmentedState st)

c = unsafePerformIO (openConsole True True)

--newWindow nm = createnamedWindow c nm
newWindow nm = createWindow c

--newsrc :: Video -> Size -> Int -> Src clk Pos
--newsrc vidsrc sz res =
--    (newsrcC vidsrc sz res) >>> (sp1 colorToBWImage)

newsrcC vidsrc sz res = 
    spIO1 (\pos -> acquireImage NoOptions vidsrc sz
                     (iscale2 res `compose2` pos))


-->>-->> IP: is using scanlE right here ?????
scanCStream f a (CStream as)= CStream $ scanlE f a as

withErrorP (a,err) = WithError a err

-->>-->> IP: This does not work since Transform2 is not instance of Num.

{-
integralS :: Num a => a -> CStream clk a -> CStream clk a
integralS = scanCStream (+)

-- stateIntegral:: Num st => st -> SP clk (st,err) (st,err)
stateIntegral:: Num st => st -> SP clk (AugmentedDelta st) (AugmentedState st)
stateIntegral start =
         \x -> joinWithE (integralS start ((lift1S fst') x), (lift1S snd') x)
         where
         fst' (WithError a err) = a
         snd' (WithError a err) = err
         joinWithE =  sp1 withErrorP . join2

basicTracker:: Num a => Stepper clk a -> Src clk a -> a -> Tracker clk a
basicTracker step src start =
     src >>> step >>> (stateIntegral start)

-- instance Num Transform2 where
-- (+) = compose2

-- ssd :: ImageInt -> Src clk Transform2 -> Transform2 -> Tracker clk Transform2
ssd im src pos = basicTracker
                     ( (lift1S (ssdStep im)) >>> (sp1 withErrorP) )
                     src pos
-}



type Plus a = a -> a -> a

integralS :: Plus a -> a -> CStream clk a -> CStream clk a
integralS f = scanCStream f

stateIntegral:: Plus st -> st -> SP clk (AugmentedDelta st) (AugmentedState st)
stateIntegral f start = 
         \x -> joinWithE (integralS f start ((lift1S fst') x), (lift1S snd') x)
         where
         fst' (WithError a err) = a
         snd' (WithError a err) = err
         joinWithE =  sp1 withErrorP . join2


basicTracker:: Plus a -> Stepper clk a -> Src clk a -> a -> Tracker clk a
basicTracker f step src start =
     src >>> step >>> (stateIntegral f start)


ssd :: ImageInt -> Src clk Transform2 -> Transform2 -> Tracker clk Transform2
ssd im src pos = basicTracker 
                     compose2 
                     ( (lift1S (ssdStep im)) >>> (sp1 withErrorP) ) 
                     src pos

pointsToTr2 :: Point2 -> Point2 -> Transform2
pointsToTr2 p1 p2 = translate2 (p2v (p2-p1)) 

masked th rgb im = threshBWImageInt th 1 (clrproject rgb (bwToColorImage im))

colorStepper:: RGBtriple -> Integer -> Stepper clk Transform2
colorStepper rgb th imS =(sp1 findTr2) imS
                         where
                          p1 im=centroidBWImageInt (masked th rgb im)
                          p2 im= let (x,y)=sizeOf im 
                                 in (x/2,y/2)
                          findTr2 im = pointsToTr2 (p1 im) (p2 im)

type Src2 clk st  = SP clk (ImageRGB,st) ImageInt
type Tracker2 clk st = SP clk (ImageRGB,st) (AugmentedState st)

colorTracker:: RGBtriple -> Integer -> Src2 clk Transform2 
               -> Tracker2 clk Transform2
colorTracker rgb th src start=
     (src >>> (colorStepper rgb th) >>> (stateIntegral compose2 start))

runColorTracker th src= do
  { c <- openConsole True True
  ; (v,_) <- openVideo "MPEG"
  ; (p,size) <- interactiveInitRect c v "Rectangle"
  ; print ("point : " ++ show p ++ " size : " ++ show size)
  ; (im,start) <- acquireImage NoOptions v (toSize size) (translate2 (p2v p))
  ; closeVideo v
  ; let rgb = avgColorImageRGB im
  ; s <- createMPEGSourceDisplay "mpeg_file.mpg"
  ; (_,img,_) <- (fst s)
  ; print ("rgb : "++ show rgb)
  ; let ct = colorTracker rgb th src start
  --; let loop imrgb=loop (
  ; runVisionSP s
      ( (loopCS start id ct) 
        (lift1S iPointToPoint2) >>>  (nulltestCS)
      )
  }


