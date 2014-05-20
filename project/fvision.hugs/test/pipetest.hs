
-- This testing file is obsolete

module PipeTest
	( module PipeTest
	) where

import Pipe
import IOExts
import XVision
import Prelude hiding(sum,or)
import Monad
import Maybe
import Overload
import XVUtilities
import List( transpose )

import Array

----------------------------------------------------------------
-- test
----------------------------------------------------------------

dot :: Pipe a -> Pipe a
dot = liftIO1 (\ a -> do { putChar '.'; return a })

-- difference between consecutive frames
dt :: Pipe Image_Int -> Pipe Image_Int
dt p = (lift2 diffImage) (advance p) p

-- difference between frames t and t-2
dt2 :: Pipe Image_Int -> Pipe Image_Int
dt2 p = (lift2 diffImage) p (advance (advance p))

-- average of two consecutive frames
avg :: Pipe Image_Int -> Pipe Image_Int
avg p = (lift2 $ avgImage 1 1) p (advance p)

-- sum of all previous frames - weighted so that old frames dominate
avg' :: Int -> Pipe Image_Int -> Pipe Image_Int
avg' w = scanPipe1 (avgImage w 1)

-- METEOR_MONO possibly caused a kernel crash and destroyed a hard disk
-- probably best not to use it for now.  (other METEOR modes should be
-- fine for now)
-- videoDevice = "METEOR_MONO"

--videoDevice = "MPEG"
--videoDevice = "METEOR_COLOR16"
videoDevice = "METEOR_COLOR24"
--videoDevice = "METEOR_MONO"

t f = do
  { c <- openConsole True True
  ; (v,_) <- openVideo videoDevice
  ; w <- createWindow c
  ; openWindow w (size v) "Output"
  ; runPipe
      $ lift1 (showImage w)
      $ lift1 bwToColor 
      $ f 
      $ lift1 colorToBW 
      $ liftIO0 (grabV v (size v) (0,0))
  }

t' sz = do
  { c <- openConsole True True
  ; (v,_) <- openVideo videoDevice
  ; w <- createWindow c
  ; openWindow w (sz + (10,10)) "Output"
  ; runPipe
      $ lift1 (wait 1000)
      $ lift1 (showImage w)
      $ liftIO0 (grabV v sz (300,200))
  }

-- hack to slow down the loop
wait :: Int -> a -> a
wait n a = if length [1..n] == -1 then a else a

t1 = runPipe $ lift1 print $ tim
tim = (1+) $ dot $ (delay 0 tim)

t2 = do
  { c <- openConsole True True
  ; (v,_) <- openVideo videoDevice

  ; let sz = (20,20)
  ; w <- createWindow c
  ; openWindow w sz "Output"
  ; runPipe $
      let
        draw  = lift1 (showImage w) $ lift1 bwToColor $ pics
	pics  = liftIO1 (grab v sz) $ posns 
        posns = lift0 (100,100,0) + lift1 (f.fromInt) time
      in
      lift1 (wait 1000) $
      draw
  }
 where
--  f t = (r * cos (t/s), r * sin (t/s), 0)
  f t = (0,0,t/s)
  r = 50
  s = 10

ptrace :: (Show a) => Pipe a -> Pipe b -> Pipe b
ptrace = liftIO2 (\a b -> do { print a; return b })

t3 = do
  { c <- openConsole True True
  ; (v,_) <- openVideo videoDevice
  ; ~[((x,y),sz)] <- interactive_init c v 1
  ; let (w,h) = (fromInt (fst sz), fromInt (snd sz))
  ; let posn0 = (fromDouble x +w/2, fromDouble y +h/2, 0)
  ; ref0 <- grab v sz posn0

  ; w <- createWindow c
  ; openWindow w sz "Output"

  ; runPipe $
      let
        pic   = liftIO1 (grab v sz) $ posn 
        posn  = integral posn0 delta
        delta = lift1 (mkPosition . ssd ref0) (delay ref0 pic)
      in
      ptrace (lift2 (,) posn delta) $ 
      lift1 (showImage w) $ lift1 bwToColor $ pic
  }


grab v sz posn = do { i <- imageVideo2 v sz 1 posn; return (colorToBW i) }

----------------------------------------------------------------
-- Static Image ops
----------------------------------------------------------------

rectangle :: Video -> Sz -> Point -> IO ()
rectangle v sz (x0,y0) = do
  { line v (x0,y0) (x0,y1)
  ; line v (x0,y1) (x1,y1)
  ; line v (x1,y1) (x1,y0)
  ; line v (x1,y0) (x0,y0)
  }
 where
  (x1,y1) = (x0,y0) + sz

avgImage :: Int -> Int -> Image_Int -> Image_Int -> Image_Int
avgImage w1 w2 i1 i2 = divInt (w1+w2) $ timesInt w1 i1 + timesInt w2 i2

diffImage :: Image_Int -> Image_Int -> Image_Int
diffImage i1 i2 = timesInt 4 $ absImage $ i1 - i2

grabV :: Video -> Sz -> Point -> IO Image_RGB
grabV v sz pos = imageVideo v pos (pos + sz) (1,1)

grabV' :: Video -> Sz -> (Double,Double) -> IO Image_Int
grabV' v sz pos = map colorToBW $ grabV v sz (round2 pos)

round2 :: (Double,Double) -> (Int,Int)
round2 (a,b) = (round a, round b)

inverse :: (Double,Double,Double,Double) -> (Double,Double,Double,Double)
inverse (a,b,c,d) = (d/det, -c/det, -b/det, a/det)
 where
  det = a*d - b*c

compressxy :: Image_Int -> Image_Int
compressxy = compressx . compressy

----------------------------------------------------------------
-- Tracking code
----------------------------------------------------------------

interactive_init :: Console -> Video -> Int -> IO [((Double,Double), (Int,Int))]
interactive_init c v n = do 
  { w <- createWindow c
  ; openWindow w (size v) "Select Object"
  ; ps <- mapem [1..n] (\_ -> get w)
  ; closeWindow w
  ; destroyWindow w
-- SSD seems to flip out on non-square images
--  ; return ((fromInt x0, fromInt y0), (x1-x0, y1-y0))
  ; return ps
  }
 where
  fudge = 4
  fix x = abs (fudge * (x `div` fudge))  -- hack: must be +ve and even
  get w = do
    { ((x0,y0),(w,h)) <- get_region w v "Select Object"
--    ; return ((fromInt x0, fromInt y0), (fix w, fix h))
    ; return ((fromInt x0, fromInt y0), (fix w, fix w)) -- hack - return square
    }

----------------------------------------------------------------
-- SSD in xy directions
----------------------------------------------------------------

ssd2 :: Int -> Image_Int -> Image_Int -> (Double,Double)
ssd2 s r0 = 
   \ r -> let dR = compressxy (fr0 - f r)
	      dx = dR `innerProduct` dX
	      dy = dR `innerProduct` dY
	      ox = a*dx + b*dy
	      oy = c*dx + d*dy
	  in
	  (fromInt s * ox, fromInt s * oy)
 where
  f = reduce_resolution (s,s)

  fr0  = f r0

  dX   = smoothDx fr0
  dY   = smoothDy fr0

  dXdX = dX `innerProduct` dX
  dXdY = dX `innerProduct` dY
  dYdX = dY `innerProduct` dX
  dYdY = dY `innerProduct` dY

  (a,b,c,d) = inverse (dXdX, dXdY, dYdX, dYdY)

----------------------------------------------------------------
-- SSD
----------------------------------------------------------------

--ssd :: Matrix -> Image_Int -> Image_Int -> ColVector

-- ToDo: add enough colVector/rowVector support to let me write:
-- cX (c,r) = column [-hc..hc]       * row (replicate r 1)
-- cY (c,r) = column (replicate c 1) * column [-hr..hr]

cX :: Sz -> Matrix
cX (c,r) = matrix (c,r) (replicate r [-hc..])
 where
  hc = fromInt ((c-1) `div` 2)

cY :: Sz -> Matrix
cY (c,r) = transpose_M (cX (r,c))

mtMatrix :: Sz -> Matrix
mtMatrix sz = unsafePerformIO (emptyMatrix sz)

mkSSD :: Image_Int -> Matrix
mkSSD ref0 = inverse_M (m_t `multMatrix` m) `multMatrix` m_t
 where
  (x,y) = size ref0 - (1,1)
  r = x * y
  dx = smoothDx ref0
  dy = smoothDy ref0
  dt = dx `productImage` cY (x,y) - dy `productImage` cX (x,y)
  m = mtMatrix (0,r) 
      `addColumn2` dx
      `addColumn2` dy
      `addColumn2` dt
  m_t = transpose_M m

ssd :: Image_Int -> Image_Int -> [Float]
ssd r0 = \ r -> 
           let
             error = imageToColVector (compressxy (r0 - r))
             delta = m0 `multColVector` error
           in
           head (fromMatrix delta)
 where
  m0 = mkSSD r0

mkPosition :: [Float] -> (Float,Float,Float)
mkPosition [x,y,theta] = (x,y,0)

----------------------------------------------------------------
-- Matrix support
----------------------------------------------------------------

matrix :: (Int,Int) -> [[Float]] -> Matrix
matrix (r,c) xs = unsafePerformIO $
  do{ m <- emptyMatrix (c,r)
    ; mapM_ (setMatrix m) (zip ixs (concat (map (take r) xs)))
    ; return m
    }
 where
  ixs = [0..c-1] `cross` [0..r-1]

fromMatrix :: Matrix -> [[Float]]
fromMatrix m = [ [ getMatrix m (x,y) | y <- [0..r-1] ] | x <- [0..c-1] ]
 where
  (c,r) = size m

cross :: [a] -> [b] -> [(a,b)]
cross as bs = [(a,b) | a <- as, b <- bs]


----------------------------------------------------------------
-- End
----------------------------------------------------------------
