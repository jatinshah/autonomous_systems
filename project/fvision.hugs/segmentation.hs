
-- Old code not currently in FVision

import Prelude hiding (sum)
import IOExts
import Window
import FVUtils

import XVTypes
-- import Monad
import Mpeg
import Video
import Image
import FVision
import FVisionGraphics
import GeometryStatic
-- import SP
import Init
import XVision(clrproject, multImageInt)
import FRPCore



-- Stream processors are defined using the Arrow class of new FRP.
--
-- type ImageSP a b = IEvent a -> IEvent b
--
-- This is a more simple approach but does not work or I can't make it work

newtype ImageSP a b = ImageSP (IEvent a -> IEvent b)

instance Arrow ImageSP where
  arr f = ImageSP (lift1 f)
  ImageSP sp1 >>> ImageSP sp2 = ImageSP (\as -> sp2 (sp1 as))
  left (ImageSP sp) = ImageSP (\abs -> let as = lift1 fst abs
		                           bs = lift1 snd abs
			               in
				           pairZ (sp as) bs ) 
                                       
runSP :: ImageSP a b -> IEvent a -> IEvent b
runSP (ImageSP sp) st = sp st

runSPonImages :: IO (IO (Maybe (Time,FVInput)),Graphic -> IO ()) ->
                 ImageSP ImageRGB Graphic -> IO ()
runSPonImages s sp= runVision s $
                      runSP sp imageSource


type AugmentedState st = WithError st

{- We need a generic notion of "Image" that include ImageRGB and
   ImageInt to replace inputImageType -}

type SegmentationStepper clk inputImageType = ImageSP inputImageType 
                                              (AugmentedState Point2)

type Src clk st = ImageSP (ImageRGB,st) ImageRGB

type SegmentationTracker clk = ImageSP (ImageRGB,Point2) 
                                      (AugmentedState Point2)


{- GDH --- need to figure out how to handle the error stuff  properly -}

threshBWImageS :: Int -> Int -> ImageSP ImageInt ImageInt
threshBWImageS thresh val = arr (threshBWImageInt thresh val)

threshBWImageS2 :: ImageSP ImageInt ImageInt
threshBWImageS2 = arr (threshBWImageInt 150 255)

{- Need to make this zero at the center of the image (or at least have
   that option -}

centroidWErrorImageS :: ImageSP ImageInt (AugmentedState Point2)
centroidWErrorImageS = arr (withErrorP . centroidP2BWImageInt) 

centroidImageS :: ImageSP ImageInt Point2
centroidImageS = arr (fst.centroidP2BWImageInt)

motionS :: ImageInt -> ImageSP ImageInt ImageInt
motionS v0 = ImageSP $ (diff >>> abs)
             where
	       diff = \x -> ((delayCE v0 x) - x)

colorS :: RGBtriple -> Int -> ImageSP ImageRGB ImageInt
colorS rgb th = arr (\rgbim ->
                        let mask=threshBWImageInt th 1 (clrproject rgb rgbim)
                        in multImageInt mask (colorToBWImage rgbim))

blobCoordinatesImageInt :: Int -> ImageSP ImageInt (AugmentedState Point2)
blobCoordinatesImageInt thresh =   (threshBWImageS thresh 1) >>> centroidWErrorImageS

-- blobCoordinatesImageInt thresh =   threshBWImageS2 >>> centroidWErrorImageS



createSrc :: ISize -> Point2 -> Src clk Vector2
createSrc size p= arr f       
                   where
                   f = \(imrgb,v) -> let im = colorToBWImage imrgb
                                         (x,y) = vector2XYCoords v
                                         (x0,y0) = point2XYCoords p
                                         x2 = round x0 + round x
                                         y2 = round y0 + round y
                                         subim = subImageInt size (x2,y2) im
                                     in
                                       bwToColorImage subim

createSegStepper :: (ImageSP inputImageType ImageInt) -> Int ->
                    SegmentationStepper clk inputImageType
createSegStepper segmenter thresh = segmenter >>> 
                                    (blobCoordinatesImageInt thresh)

motionBlobImageInt :: Int -> ImageInt -> SegmentationStepper clk ImageInt
motionBlobImageInt thresh v0 = createSegStepper (motionS v0) thresh


colorBlobImageRGB :: Int -> RGBtriple -> SegmentationStepper clk ImageRGB
colorBlobImageRGB thresh rgb = createSegStepper (colorS rgb thresh) thresh

colorTr::Int -> ImageRGB -> RGBtriple -> ISize -> Point2 ->
         ImageSP ImageRGB (AugmentedState Vector2)

colorTr thresh im rgb size origin = ImageSP f
  where
    f imrgbs = loopSP2 (im,zeroVector) g imrgbs track 
    track = (createSrc size origin) >>> (colorBlobImageRGB thresh rgb) >>>
            integ
    g (WithError p err) rgbim = (rgbim,p)

{- It is not nice to convert the BW image to color at the source  just
   before returning it and then convert it back to BW again here but I assume
   that we will find some way to take subimages of color images. 
   
   In fact this code is almost same with colorTr and could be generalized if
   both the steppers were using the same kind of image -}

motionTr::Int -> ImageRGB -> ISize -> Point2 -> ImageInt ->
         ImageSP ImageRGB (AugmentedState Vector2)

motionTr thresh im size origin im0 = ImageSP loop
    where
        loop imrgbs = loopSP2 (im,zeroVector) f imrgbs track
        track = (createSrc size origin) >>> (arr colorToBWImage) >>>
	        (motionBlobImageInt thresh im0) >>>
	        integ
        f (WithError p err) rgbim = (rgbim,p)


runMotionTracker thresh = do
  { c <- openConsole True True
  ; (v,_) <- openVideo "MPEG"
  ; (p,size) <- interactiveInitRect c v "Rectangle"
  ; (im,_) <- acquireImage NoOptions v (toSize size) (translate2 (p2v p))
  ; let im0 = colorToBWImage im
  ; print ("p : "++ show p ++ "size : " ++ show size)
  ; closeVideo v
  ; let s = createMPEGSourceDisplay "mpeg_file.mpg"
  ; s' <- s
  ; Just(t,inp) <- (fst s')
  ; let im = theImage inp
  ; print ("*** image : " ++ show (sizeOf im))
  ; let motionTracker = motionTr thresh im size p im0
  ; runSPonImages s $
      motionTracker >>> printStream >>>
      arr (drawRect p $ iPointToPoint2 size)
  }





runColorTracker thresh = do
  { c <- openConsole True True
  ; (v,_) <- openVideo "MPEG"
  ; (p,size) <- interactiveInitRect c v "Rectangle"
  ; (im,_) <- acquireImage NoOptions v (toSize size) (translate2 (p2v p))
  ; let rgb = avgColorImageRGB im
  ; print ("rgb : "++ show rgb)
  ; (p,size) <- interactiveInitRect c v "Rectangle"
  ; print ("p : "++ show p ++ "size : " ++ show size)
  ; closeVideo v
  ; let s = createMPEGSourceDisplay "mpeg_file.mpg"
  ; s' <- s
  ; Just(t,inp) <- (fst s')
  ; let im = theImage inp
  ; print ("*** image : " ++ show (sizeOf im))
  ; let colorTracker = colorTr thresh im rgb size p
  ; runSPonImages s $
       colorTracker >>> printStream >>>
       arr (drawRect p $ iPointToPoint2 size)
  }



runColorTracker2 thresh = do
  { c <- openConsole True True
  ; (v,_) <- openVideo "MPEG"
  ; (p1,size1) <- interactiveInitRect c v "Rectangle"
  ; (im,_) <- acquireImage NoOptions v (toSize size1) (translate2 (p2v p1))
  ; let rgb = avgColorImageRGB im
  ; print ("rgb : "++ show rgb)
  ; (p,size) <- interactiveInitRect c v "Rectangle"
  ; print ("p : "++ show p ++ "size : " ++ show size)
  ; closeVideo v
  ; let s = createMPEGSourceDisplay "mpeg_file.mpg"
  ; s' <- s
  ; Just(t,inp) <- (fst s')
  ; let im = theImage inp
  ; print ("*** image : " ++ show (sizeOf im))
  ; let colorTracker = colorTr thresh im rgb size p
  ; runSPonImages s $
      colorBlobImageRGB thresh rgb >>>  integ >>>
      printStream >>>
      arr (drawRect p $ iPointToPoint2 size)
  }

{-

shift :: (Int,Int) -> ImageSP (WithError Point2) (WithError Vector2)
shift (i1,i2) = lift1 (\(WithError p err) -> let x = (fromInt i1*0.5) 
		                                 y = (fromInt i2*0.5) 
						 v = p .-. Point2XY x y
				 	     in WithError v err)

-}


testLoop = do
  { let s = createMPEGSourceDisplay "mpeg_file.mpg"
  ; s' <- s
  ; Just(t,inp) <- (fst s')
  ; let im0 = colorToBWImage (theImage inp)
  ; print ("*** image : "++show im0)
  ; runSPonImages s $
      arr colorToBWImage >>>
      ImageSP (meanims im0) >>>
      draw
  }

meanims :: ImageInt -> IEvent ImageInt -> IEvent ImageInt
meanims im0 ims= means
                  where means = delayCE im0 (lift1 (iDivImageInt 2) $
		                             lift2 (+) means ims)

draw :: ImageSP ImageInt Graphic
draw = arr (\im -> let p = fst (centroidP2BWImageInt im)
                   in GLine (Point2XY 1 1) p)
                   






-- New names in FRP. Noted here to change other FVision modules.

p2v = point2ToVector2
v2p = vector2ToPoint2

-- Utilities

withErrorP (a,err) = WithError a err


loopSP2 :: (a,b) -> (c -> a -> (a,b)) -> IEvent a -> ImageSP (a,b) c -> 
           IEvent c 
loopSP2 (a0,b0) f aCS sp = let b = runSP sp (delayCE (a0,b0) a)
                               a = lift2 f b aCS
                           in b

drawRect :: Point2 -> Point2 -> AugmentedState Vector2 -> Graphic
drawRect p size (WithError v err) = GRect (p .-^ v) (p .+^ (p2v size ^-^ v))

--drawRect p (WithError v err) = GLine (Point2XY 320 240) v -- (Point2XY 40 40)
drawLine (WithError v err) = GLine (Point2XY 10 10) (v2p v)

printStream :: Show a => ImageSP a a
printStream = arr (\x -> unsafePerformIO (do { print x ; return x }))

integ :: ImageSP (AugmentedState Point2) (AugmentedState Vector2) 
integ  =  ImageSP $
          (\p_err ->let ps2 = delay1E ps
                        vs = lift2 (.-.) ps2 ps
                        ps = (lift1 fst') p_err
                        err = (lift1 snd') p_err
                        fst' (WithError a err) = a
                        snd' (WithError a err) = err
                    in
                      (lift2 WithError) (scanE (+) zeroVector vs) err )

