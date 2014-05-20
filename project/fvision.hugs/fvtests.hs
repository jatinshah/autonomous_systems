
module FVTests where

import FVision
import GraphicsZ
import GeometryZ
import qualified GeometryStatic as GS
import qualified Graphics as G
import FRP
import FRPSignal
import FRPTask
import qualified NXVision2 as XV
import IOExts (unsafePerformIO)


dotAt c p = withColor c $ moveTo p $ stretch 10 $ circle

testF p = runmpeg "test.mpg" "" (showPic p)
test p = runmpeg "firewire" "" (showPic p)

main   = test emptyPic
main1  = test (dotAt red (point2XY (50*timeB) (30*timeB)))
main2  = test (dotAt red mouseB)
main3  = test (dotAt (cycleColors [red, white, blue] lbpE) mouseB)
main4  = test (dotAt (cycleColors [red, white, blue] rbpE) mouseB)

--cycleColors colors e = cc1 (cycle colors)
-- where cc1 (c:cs) = c `till` e -=> cc1 cs

cycleColors colors e = switch (head colors) (tagE_ e (tail (cycle colors)))

followMe :: VBehavior Picture
followMe = emptyPic `till` lbpPosE ==>
             (\corner1 -> rectangle (lift0 corner1) mouseB
                `till` lbpPosE ==> 
                 (\corner2 -> fillRectangle (lift0 corner1) (lift0 corner2)))


track :: VBehavior Picture
track = emptyPic `till` lbpPosE ==>
             (\corner1 -> rectangle (lift0 corner1) mouseB
                `till` (lbpPosE `snapshotE` inputB) ==> 
                 (\(corner2,inp) -> 
                      let (x1,y1)=GS.point2XYCoords corner1 
                          (x2,y2)=GS.point2XYCoords corner2 
                          (x,y)=(round x1,round y1)
                          (dx,dy)=(round (x2-x1), round (y2-y1))
                          im = XV.getRegion (viImage inp) x y dx dy
                          c  = XV.getColorSelector im
                          f img = let (xc, yc, area) = XV.stepBlob (viImage img) c
                                      area' = (round (area*0.5)) :: Int
                                      (x,y)=(round xc,round yc)
                                  in ( ipoint2XY (x - area') (y - area'),
                                       ipoint2XY (x + area') (y + area') )
                          (corner1B, corner2B) = pairZSplit $ lift1 f inputB
                      in
                       rectangle corner1B corner2B))

capture :: Behavior inp XV.ImageRGB -> XV.ColorSelector -> Event inp XV.ImageRGB
capture ims c = snapshotE_  (whenE (lift2 change posB pos2B <* close)) ims 
            where
            posB = lift2 XV.stepBlob ims (lift0 c)
            pos2B = delayNB 10 posB
            close = lift0 (1,1,4)
            change (x1,y1,a1) (x2,y2,a2) = if (a1==0) || (a2==0)
                                           then (100,100,100)
                                           else (abs(x2-x1),abs(y2-y1),abs(a2-a1))
            delayNB 1 b = delay1B b
            delayNB n b = delay1B $ delayNB (n-1) b

gesture =  emptyPic `till` lbpPosE ==> 
             (\corner1 -> rectangle (lift0 corner1) mouseB
                `till` (lbpPosE `snapshotE` inputB) ==>
                 (\(corner2,inp) -> 
                     let (x1,y1)=GS.point2XYCoords corner1
                         (x2,y2)=GS.point2XYCoords corner2
                         (x,y)=(round x1,round y1)
                         (dx,dy)=(round (x2-x1), round (y2-y1)) 
                         im = XV.getRegion (viImage inp) x y dx dy
                         c  = XV.getColorSelector im
                         subims x y x' y' = lift5 XV.getRegion imageB (lift0 x) 
                                                  (lift0 y) (lift0 (x'-x)) 
                                                  (lift0 (y'-y))
                      in 
                          rectangle (lift0 (ipoint2XY c1x c1y)) 
                                    (lift0 (ipoint2XY c2x c2y))
                          `till` (capture (subims c1x c1y c2x c2y) c 
                                  -=> emptyPic) 
                      ))
            where
              (c1x,c1y,c2x,c2y) = (400,250,600,400)


imageB :: Behavior VideoInput XV.ImageRGB
imageB = lift1 viImage inputB

ipoint2XY :: Int -> Int -> Point2
ipoint2XY i1 i2 = GS.point2XY (fromInt i1) (fromInt i2)

rectangle p1 p2 = withColor white $
                  polyline [p1, p3, p2, p4] 
  where p3 = point2XY (point2XCoord p1) (point2YCoord p2)
        p4 = point2XY (point2XCoord p2) (point2YCoord p1)

rectangle' p1 p2 = polyline [p1, p3, p2, p4] 
  where p3 = point2XY (point2XCoord p1) (point2YCoord p2)
        p4 = point2XY (point2XCoord p2) (point2YCoord p1)

rect' p1 p2 = G.polyline [p1, p3, p2, p4]
    where p3 = GS.point2XY (GS.point2XCoord p1) (GS.point2YCoord p2)
          p4 = GS.point2XY (GS.point2XCoord p2) (GS.point2YCoord p1)


fillRectangle p1 p2 = withColor white $
                      polygon [p1, p3, p2, p4] 
  where p3 = point2XY (point2XCoord p1) (point2YCoord p2)
        p4 = point2XY (point2XCoord p2) (point2YCoord p1)

main5 = test followMe
main6 = test track
mainG = test gesture

-- FVision Tasks

type VisionTask e = SimpleTask VideoInput Picture e
type IRegion = (Int,Int,Int,Int)

runVisionTask t = test (runSimpleT_ t emptyPic)

getImage :: IRegion -> VisionTask XV.ImageRGB
getImage (c1x,c1y,c2x,c2y) = 
  do mkTask (rectangle (lift0 (ipoint2XY c1x c1y)) (lift0 (ipoint2XY c2x c2y))) lbpE
     im0 <- snapshotNowT imageB
     return (XV.getRegion im0 c1x c1y (c2x-c1x) (c2y-c1y))
     
gesture' = do let reg1 = (400,250,520,400)
                  reg2 = (400,100,520,250)
                  reg  = (400,100,600,400)
              im11 <- getImage reg1
              im12 <- getImage reg1
              im13 <- getImage reg1
              im14 <- getImage reg2
              im21 <- getImage reg1
              im22 <- getImage reg1
              im23 <- getImage reg1
              im24 <- getImage reg2
              liftT $  findG [im11,im12,im13,im21,im22,im23] reg1 
                       -- `over` findG [im12,im14,im22,im24] reg2


findG' :: [XV.ImageRGB] -> IRegion -> XV.ImageRGB -> Color
findG' temps reg im =
  let [f11,f12,f13,f21,f22,f23] = map XV.compareImageInt temps
      (c1x,c1y,c2x,c2y) = reg
      subim = XV.getRegion im c1x c1y (c2x-c1x) (c2y-c1y)
      err1 = f11 subim + f12 subim + f13 subim
      err2 = f21 subim + f22 subim + f23 subim
      b = unsafePerformIO $ do{ print (show err1 ++ " : " ++ show err2 ++ 
                                       if (err1<err2) then " -> 1ST" else " -> 2ND"); return 5 }
  in
      if (b==5) then 
        if (err1<err2) then Red else Blue
      else White


{- 
findG' :: [XV.ImageRGB] -> IRegion -> XV.ImageRGB -> Color
findG' temps reg im = 
  let [ssd11,ssd12,ssd13,ssd21,ssd22,ssd23] = map XV.createSSDstepper temps
      (c1x,c1y,c2x,c2y) = reg
      subim = XV.getRegion im c1x c1y (c2x-c1x) (c2y-c1y)
      err1 = XV.compareSSD ssd11 subim + XV.compareSSD ssd12 subim + XV.compareSSD ssd13 subim
      err2 = XV.compareSSD ssd21 subim + XV.compareSSD ssd22 subim + XV.compareSSD ssd23 subim
      b = unsafePerformIO $ do{ print (show err1 ++ " : " ++ show err2 ++
                                       if (err1<err2) then " -> 1ST" else " -> 2ND"); return 5 }
  in
      if (b==5) then 
        if (err1<err2) then Red else Blue
      else White
-}

findG temps reg =
  let (c1x,c1y,c2x,c2y) = reg
      rect = rectangle' (lift0 (ipoint2XY c1x c1y)) (lift0 (ipoint2XY c2x c2y))
      colorB = lift1 (findG' temps reg) imageB
  in 
      withColor colorB rect
         
main11 = runVisionTask gesture'

type DRegion = (Double, Double, Double, Double)

getCorner :: VisionTask Point2
getCorner = mkTask emptyPic lbpPosE

getColor :: VisionTask (XV.ColorSelector, DRegion)
getColor = do c1 <- mkTask emptyPic lbpPosE
              (c2,im) <- mkTask (rectangle (lift0 c1) mouseB) lbpPosE 
                         `snapshotT` imageB
              let (x1,y1)=GS.point2XYCoords c1
                  (x2,y2)=GS.point2XYCoords c2
                  (x,y)=(round x1,round y1)
                  (dx,dy)=(round (x2-x1), round (y2-y1))
                  c  = XV.getColorSelector (XV.getRegion im x y dx dy)
              return (c,(x1-10.0,y1-10.0,(x2-x1+10.0),(y2-y1+10.0)))

getRegion :: XV.ImageRGB -> DRegion -> XV.ImageRGB
getRegion im (x,y,w,h) = XV.getRegion im (round x) (round y) (round w) (round h)

trackT = 
  do (c,reg0) <- getColor
     let regB = delayB reg0 (lift2 (findRegion c) imageB regB)                       
         f reg = let (x,y,w,h) = reg
                 in (GS.point2XY x y, GS.point2XY (x+w) (y+h))
         (c1,c2) = pairZSplit $ lift1 f regB
     liftT $ rectangle c1 c2

findRegion :: XV.ColorSelector -> XV.ImageRGB -> DRegion -> DRegion
findRegion c im reg = let (x,y,w,h) = reg
                          dist = 5.0
                          subim = getRegion im reg
                          (x2, y2, w2, h2) = toDouble $ XV.stepBlob2 subim c
                          x' = if w2<=0 then x else x+x2-dist
                          y' = if h2<=0 then y else y+y2-dist
                          w' = if w2<=0 then w else w2+2*dist
                          h' = if h2<=0 then h else h2+2*dist 
                      in
                          (x',y',w',h')


main12 = runVisionTask trackT

toDouble :: (Float,Float,Float,Float) -> (Double,Double,Double,Double)
toDouble (a,b,c,d) = (f a, f b, f c, f d)
                     where f = fromInt . round

trackT2 =
  do (c,reg0) <- getColor
     let regB = delayB reg0 (lift2 (findRegion c) imageB regB)
         f reg = let (x,y,w,h) = reg
                 in (GS.point2XY x y, GS.point2XY (x+w) (y+h))
         g reg im = let subim = getRegion im reg
                        (x0,y0,w0,h0) = reg
                        regs  = map (\(x2,y2,w2,h2) -> (x0+x2,y0+y2,w2,h2))
                                    (XV.blobRegions c subim)
                        hck = unsafePerformIO $ do { print regs; return 5 }
                    in
                        -- map f regs
                        if hck==5 then map f regs
                        else error "debug"
         rects [] = G.emptyPic
         rects ((p1,p2):ps) = G.over (rect' p1 p2) (rects ps)
     liftT $ withColor red $ lift1 rects (lift2 g regB imageB)

main14 = runVisionTask trackT2


main13  = test (dotAt red (fstZ pos_v) `over`
                dotAt blue mouseB)

pos_v :: VBehavior (Point2,Vector2)
pos_v = delayB center (lift2 f pos_v mouseB)
       where
        center  = (GS.point2XY 320 240, GS.zeroVector)
        f (p,v) p2 = (p GS..+^ (speed GS.*^ GS.normalize newv), newv)
                     where
                       vec = p GS..-.p2
                       newv = if GS.magnitude vec < radius then vec GS.^+^ oldv else oldv
                       oldv = friction GS.*^ v
                       radius = GS.magnitude (GS.Vector2XY 20 20)
                       speed  = 5   
                       friction = 0.5


