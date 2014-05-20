
-- All of this code was part of Alastair's XVision files and was deleted
-- by jcp.  

module SSD
	( module SSD
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
import qualified AnsiScreen
import Init

import Array

----------------------------------------------------------------
-- test
----------------------------------------------------------------

-- METEOR_MONO possibly caused a kernel crash and destroyed a hard disk
-- probably best not to use it for now.  (other METEOR modes should be
-- fine for now)
-- videoDevice = "METEOR_MONO"

--videoDevice = "MPEG"
--videoDevice = "METEOR_COLOR16"
videoDevice = "METEOR_COLOR24"
--videoDevice = "METEOR_MONO"

ssd_scale :: (Num a) => a
ssd_scale = 4

t = do
  { c <- openConsole True True
  ; putStrLn AnsiScreen.cls
  ; let { s = ssd_scale; s :: (Num a) => a }
  ; (v,_) <- openVideo videoDevice
  ; ~[((x,y),sz)] <- interactive_init c v 1
  ; let (w,h) = (fromInt (fst sz), fromInt (snd sz))
  ; let posn0 = (fromDouble x + w*s/2, fromDouble y +h*s/2, 0)
  ; ref0 <- grab v s sz posn0

  ; print (x,y)
  ; print (sz,size ref0)

  ; w <- createWindow c
  ; openWindow w (bigger s sz) "Output"

  ; runPipe $
      let
        error    = lift2 diffImage (lift0 ref0) pic
        pic      = liftIO1 (grab v s sz) $ posn 
        posn     = integral posn0 delta
        delta    = lift2 (rotate' s) posn rawdelta
        (rawdelta,residual) = drop2 $ lift1 (ssd ref0) pic
      in

      subsample 20 $

      ptrace' (0,10) (listToPipe [0..]) $
      --ptrace (lift3 (,,) posn delta rawdelta) $ 
      ptrace' (0,11) posn $
      ptrace' (0,12) residual $

      lift1 (wait 5000) $   -- slowdown to overcome camera problems

      lift1 (showImage w) $ lift1 bwToColor $ lift1 (scale (s,s)) pic
      --lift1 (showImage w) $ lift1 bwToColor error
      --lift0 (return ())
  }

t2 = do
  { c <- openConsole True True
  ; putStrLn AnsiScreen.cls
  ; (v,_) <- openVideo videoDevice
  ; ~[(a,a_sz),(b,b_sz)] <- interactive_init c v 2

  ; wa <- createWindow c
  ; wb <- createWindow c

  ; let a_posn0 = initPosn a a_sz
  ; let b_posn0 = initPosn b b_sz

  ; a_ref0 <- grab v s a_sz a_posn0
  ; b_ref0 <- grab v s b_sz b_posn0

  ; runPipe $
      let
        (a_posn,a_pic) = ssdLoop v a_sz a_posn0 a_ref0
        (b_posn,b_pic) = ssdLoop v b_sz b_posn0 b_ref0
      in
      subsample 20 $
      lift1 (wait 2000) $   -- slowdown to overcome camera problems
      ptrace' (0,20) (listToPipe [0..]) $
      ptrace' (0,21) a_posn $
      ptrace' (0,22) b_posn $
      ptrace' (0,23) (a_posn-b_posn) $
      lift2 (>>)
        (lift1 (showImage wa) $ lift1 bwToColor $ lift1 (scale (s,s)) a_pic)
        (lift1 (showImage wb) $ lift1 bwToColor $ lift1 (scale (s,s)) b_pic)
{-
      lift2 (>>)
        (lift0 (showLive wa v 4))
        (lift2 (showPosn wa 4) a_posn b_posn)
-}
  }
 where
  s :: (Num a) => a
  s = ssd_scale

  ssdLoop v sz posn0 ref0 = 
      let
        pic   = liftIO1 (grab v s sz) $ posn 
        posn  = integral posn0 delta
        delta = lift2 (rotate' s) posn rawdelta
        (rawdelta,residual) = drop2 $ lift1 (ssd ref0) pic
      in
      (posn,pic)

  initPosn (x,y) (w,h) = 
     (fromDouble x + fromInt w *s/2, fromDouble y + fromInt h * s/2, 0)

-- ssdLoop v sz posn0 ref0 = 
--	let
--	  pic1   = liftIO1 (grab v (s * 2) sz) $ posn 
--	  pic2   = liftIO1 (grab v s sz) $ posn + delta1 
--	  posn  = integral posn0 (delta2 + delta1)
--	  delta2 = lift2 (rotate' s) posn rawdelta2
--	  delta1 = lift2 (rotate' (s * 2)) posn rawdelta1
--	  (rawdelta2,residual2) = drop2 $ lift1 (ssd ref0) pic2
--	  (rawdelta1,residual1) = drop2 $ lift1 (ssd (reduce_resolution (2,2) ref0)) pic1
--	in
--	(posn,pic)


clown = do
  { c <- openConsole True True
  ; putStrLn AnsiScreen.cls
  ; (v,_) <- openVideo videoDevice

  ; putStrLn "Select left and right eyes"
  ; ~[(l_pos,l_sz),(r_pos,r_sz)] <- interactive_init c v 2

  ; l_w <- createWindow c
  ; r_w <- createWindow c
  ; h_w <- createWindow c

  ; let l_posn0 = initPosn l_pos l_sz
  ; let r_posn0 = initPosn r_pos r_sz

  ; putStrLn "Open both eyes"
  ; getLine
  ; l_open <- grab v s l_sz l_posn0
  ; r_open <- grab v s r_sz r_posn0

  ; putStrLn "Close both eyes"
  ; getLine
  ; l_closed <- grab v s l_sz l_posn0
  ; r_closed <- grab v s r_sz r_posn0
  ; runPipe $
      let
        (l_posn,l_pic,l_isOpen) = eye v l_sz l_posn0 l_open l_closed
        (r_posn,r_pic,r_isOpen) = eye v r_sz r_posn0 r_open r_closed
      in
      --subsample 10 $
      --lift1 (wait 2000) $   -- slowdown to overcome camera problems
      ptrace' (0,20) (listToPipe [0..]) $
      ptrace' (0,21) l_posn $
      ptrace' (0,22) r_posn $
      ptrace' (0,23) l_isOpen $
      ptrace' (0,24) r_isOpen $
      lift4 (drawHead v h_w) l_posn l_isOpen  r_posn r_isOpen 
{-
      lift2 (>>)
        (lift1 (showImage l_w) $ lift1 bwToColor $ lift1 (scale (s,s)) l_pic)
        (lift1 (showImage r_w) $ lift1 bwToColor $ lift1 (scale (s,s)) r_pic)
-}
{-
      lift2 (>>)
        (lift0 (showLive wa v 4))
        (lift2 (showPosn wa 4) l_posn r_posn)
-}
--      lift0 (return ())
  }
 where
  s :: (Num a) => a
  s = ssd_scale

  eye :: Video -> Sz -> (Double,Double,Double) -> Image_Int -> Image_Int ->
         (Pipe (Double,Double,Double), Pipe Image_Int, Pipe Bool)
  eye v sz posn0 ref_a ref_b = 
      let
        pic   = liftIO1 (grab v s sz) $ posn 
        posn  = integral posn0 delta

        delta = cond a_or_b delta_a delta_b
        a_or_b = lift2 (<) residual_a residual_b

        delta_a = lift2 (rotate' s) posn rawdelta_a
        (rawdelta_a,residual_a) = drop2 $ lift1 (ssd ref_a) pic

        delta_b = lift2 (rotate' s) posn rawdelta_b
        (rawdelta_b,residual_b) = drop2 $ lift1 (ssd ref_b) pic

      in
      (posn,pic,a_or_b)

  initPosn (x,y) (w,h) = 
     (fromDouble x + fromInt w *s/2, fromDouble y + fromInt h * s/2, 0)

drawHead :: Video -> Window -> (Double,Double,Double) -> Bool -> (Double,Double,Double) -> Bool -> IO ()
drawHead v w (x1,y1,_) open1 (x2,y2,_) open2 = do
  { head <- imageVideo2 v (200,300) 1 (x,y,0)
  ; showImage w head
  ; blob w ((x1,y1) - (x,y) + (100,150)) open1
  ; blob w ((x2,y2) - (x,y) + (100,150)) open2
  ; wait 2000 (return ())
  }
 where
  x = average x1 x2
  y = average y1 y2

blob :: Window -> (Double,Double) -> Bool -> IO ()
blob w (x,y) True  = rectangleW w ((round x, round y) - (10,30)) ((round x, round y) + (10,30))
blob w (x,y) False = rectangleW w ((round x, round y) - (30,20)) ((round x, round y) + (30,10))

center :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
center (x1,y1,a1) (x2,y2,a2) = (average x1 x2, average y1 y2, average a1 a2)

average :: Fractional a => a -> a -> a
average a b = (a+b)/2 

cond = lift3 (\ x y z -> if x then y else z)


round2 :: (Double,Double) -> (Int,Int)
round2 (a,b) = (round a, round b)

-- warning: scale factor doesn't seem to work - always use 1 for now
showLive :: Window -> Video -> Int -> IO ()
showLive w v s = do
  { i <- imageVideo v (0,0) (size v) (1,1)
  ; showImage w $ bwToColor $ reduce_resolution (s,s) $ colorToBW $ i
  }

showPosn :: Window -> Double -> (Double,Double,Double) -> (Double,Double,Double) -> IO ()
showPosn w s (ax,ay,_) (bx,by,_) = rectangleW w a b
 where
  a = bigger (1/s) $ round2 (ax,ay)
  b = bigger (1/s) $ round2 (bx,by)

bigger :: Double -> Sz -> Sz
bigger n (x,y) = (round (n * fromInt x), round (n * fromInt y))

-- used for timing tests with rotation turned off
--rotate' s _ (dx,dy,_) = (s * dx,s * dy,0)

rotate' s (_,_,theta) (dx,dy,dtheta) = (s * dx',s * dy',dtheta)
 where
  (dx',dy') = rotate (-theta) (dx,dy)

rotate theta (x,y) = (c*x - s*y, s*x + c*y)
 where
  c = cos theta
  s = sin theta

grab v s sz posn = do 
  { i <- imageVideo2 v (bigger s sz) 1 posn
  ; return $ reduce_resolution (round s, round s) $ colorToBW $ i
  }

ptrace :: (Show a) => Pipe a -> Pipe b -> Pipe b
ptrace = liftIO2 (\a b -> do { print a; return b })

ptrace' :: (Show a) => AnsiScreen.Pos -> Pipe a -> Pipe b -> Pipe b
ptrace' pos = liftIO2 (\a b -> do { putStr (AnsiScreen.at pos (show a)); return b })

diffImage :: Image_Int -> Image_Int -> Image_Int
diffImage i1 i2 = timesInt 4 $ absImage $ i1 - i2

dot :: Pipe a -> Pipe a
dot = liftIO1 (\ a -> do { putChar '.'; return a })

wait :: Int -> a -> a
wait n a = if length [1..n] == -1 then a else a

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
  fix x = abs (fudge * ((x `div` ssd_scale) `div` fudge))  -- hack: must be +ve and even
  get w = do
    { ((x0,y0),(w,h)) <- get_region w v "Select Object"
--    ; return ((fromInt x0, fromInt y0), (fix w, fix h))
    ; return ((fromInt x0, fromInt y0), (fix w, fix w)) -- hack - return square
    }

----------------------------------------------------------------
-- Matrix support
----------------------------------------------------------------

matrix :: (Int,Int) -> [[Double]] -> Matrix
matrix (r,c) xs = unsafePerformIO $
  do{ m <- emptyMatrix (c,r)
    ; mapM_ (setMatrix m) (zip ixs (concat (map (take r) xs)))
    ; return m
    }
 where
  ixs = [0..c-1] `cross` [0..r-1]

fromMatrix :: Matrix -> [[Double]]
fromMatrix m = [ [ getMatrix m (x,y) | y <- [0..r-1] ] | x <- [0..c-1] ]
 where
  (c,r) = size m

mtMatrix :: Sz -> Matrix
mtMatrix sz = unsafePerformIO (emptyMatrix sz)

colsToMatrix :: Int -> [Image_Int] -> Matrix
colsToMatrix sz is = foldl addColumn2 (mtMatrix (0,sz)) is

cross :: [a] -> [b] -> [(a,b)]
cross as bs = [(a,b) | a <- as, b <- bs]


----------------------------------------------------------------
-- End
----------------------------------------------------------------


------ Pipe.hs

----------------------------------------------------------------
-- Devices
----------------------------------------------------------------

-- I don't think it's a good idea to use this
video :: String -> Pipe Image_RGB
video name = Pipe $ unsafePerformIO $ do
  { (v,_) <- openVideo name
  ; mkLazyList (imageVideo v (0,0) (sizeOf v) (1,1))
  }

-- not too sure abotu this either
window :: Console -> (Int,Int) -> String -> Pipe Image_RGB -> Pipe (IO ())
window c sz title ims = Pipe $ unsafePerformIO $ do
  { w <- createWindow c
  ; openWindow w sz title
  ; return (map (showImage w) (unP ims))
  }

