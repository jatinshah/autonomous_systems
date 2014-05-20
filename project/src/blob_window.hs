module Main where

import Animate
import NXVision2
import FVision
import GraphicsZ
import GeometryZ
import qualified GeometryStatic as S
import FRP
import FRPSignal(delayB)
import FRPExtra
import Force


dotAt c p = withColor c $ moveTo p $ stretch 5 $ circle

play p = runmpeg "ball.mpg" "" (showPic p)


cycleColors colors e = switch (head colors) (tagE_ e (tail (cycle colors)))

blobSize :: Int
blobSize = 5

windowSize :: Float
windowSize = 20
minWindowSize :: Float
minWindowSize = 20

percent :: Float
percent = 50

epsilon :: Float
epsilon = 1

threshold :: Int
threshold = 5


toDouble x = fromRational $ toRational x
toFloat x = fromRational $ toRational x

myWatch state str x = if state==1 then (watchB (\t a -> str++show a++"\n") x) else x

adjust x w s  = let
                   a = x-(w `div` 2)>=0
                   b = (x+(w `div` 2)+(w `mod` 2))<s
               in
                   case a of 
                    True  -> if b then w else 2*(s-x-1)
                    False -> if b then 2*x else 0

adjust1 ulx w s = let
			a = (ulx>=0 && ulx<s)
			b = (ulx+w-1>=0 && ulx+w-1<s)
		 in
			case a of
			 True  -> if b then (ulx,w) else (max 0 (s-w),s-(max 0 (s-w)))
			 False -> if b then (0,(min w s)) else (0,s)

changeWinSz windowSz percent maxWindowSize | percent >=0 =  min maxWindowSize (windowSz + percent*windowSz/100)
			                   | percent <0  =  max minWindowSize (windowSz + percent*windowSz/100) 

rectangle p1 p2 = withColor white $ 
		  polyline [p1,p3,p2,p4]
	where
		p3 = point2XY (point2XCoord p1) (point2YCoord p2)
       		p4 = point2XY (point2XCoord p2) (point2YCoord p1)

forceInput b = lift2 seq (fmap force inputB) b

forceIO b = lift2 seq (fmap force inputB) (fmap force b)

followMe :: VBehavior Picture
followMe =
    emptyPic
    `till`
    (lbpE `snapshotE_` (pairZ mouseB videoB)) ==>
        (\(center,image) ->
            let (sizeY,sizeX)=imageSize image
		maxWindowSize = fromIntegral $ max sizeX sizeY
	        centerX = toFloat $ S.point2XCoord center
		centerY = toFloat $ S.point2YCoord center
		-- Upper left and Lower right corners of this window
		ulX=centerX-(fromIntegral blobSize)/2
		ulY=centerY-(fromIntegral blobSize)/2

		(locX,blobWindowX) = adjust1 (truncate ulX) blobSize sizeX
		(locY,blobWindowY) = adjust1 (truncate ulY) blobSize sizeY
                blob = getRegion image
				 locX
				 locY
				 (blobWindowX-1)		-- Should be just blobWindowX
				 (blobWindowY-1)		-- Error in XVision2

		color = getColorSelector blob
		newXY = loopB (\newXYref ->
		    let 
		        position0X = (centerX) `delayB` (sel5_1Z (derefB newXYref))
			position0Y = (centerY) `delayB` (sel5_2Z (derefB newXYref))
			position1X = (centerX) `delayB` position0X
			position1Y = (centerY) `delayB` position0Y
			windowSz = windowSize `delayB` (sel5_3Z (derefB newXYref))
			noBlobCount  = 0 `delayB` (sel5_4Z (derefB newXYref))
			blobCount = 0 `delayB` (sel5_5Z (derefB newXYref))
			time0	  = 0 `delayB` timeB
			time1 	  = 1 `delayB` time0

			deltaTime=time0-time1
			dispX=position0X-position1X
			dispY=(position0Y-position1Y)
			predictedPositionX= (position0X+dispX)
			predictedPositionY= (position0Y+dispY)

			-- Upper left corner of window at estimated position

			predictedUlX = predictedPositionX - (windowSz)/2
			predictedUlY = predictedPositionY - (windowSz)/2

			(locationX,windowX) = pairZSplit $ lift2 (\p q -> adjust1 p q sizeX) (lift1 truncate (myWatch 0 "pUlX: " predictedUlX)) (lift1 round (myWatch 0 "WinSz: " windowSz))
			(locationY,windowY) = pairZSplit $ lift2 (\p q -> adjust1 p q sizeY) (lift1 truncate (myWatch 0 "pUY: " predictedUlY)) (lift1 round windowSz)

			window = lift5 getRegion
			            (lift1 viImage (lift1 ciBase inputB))
				    locationX
				    locationY
				    (windowX-1)
				    (windowY-1)
			(blob2X,blob2Y,blob2W,blob2H) =
			    untuple4Z (lift2 stepBlob2 window (lift0 color))

	--		correctionX=(blob2X+blob2W/2)-(lift1 fromInt windowX)/2
	--		correctionY=(blob2Y+blob2H/2)-(lift1 fromInt windowY)/2

			dataError = (blob2X<*0 ||* blob2Y<*0 ||* blob2W<*0 ||* blob2H<*0)

			newX =  ifZ dataError position0X (lift1 fromIntegral locationX + blob2X + blob2W/2)
			newY =  ifZ dataError position0Y (lift1 fromIntegral locationY + blob2Y + blob2H/2)
			newnoBlobCount = ifZ dataError (noBlobCount+1) 0
			newBlobCount = ifZ dataError 0 (blobCount+1)
			newWinSz = ifZ (newnoBlobCount >* (lift0 threshold)) 
					(lift3 changeWinSz windowSz (lift0 percent) (lift0 maxWindowSize))
					(ifZ (newBlobCount >* (lift0 threshold)) 
						(lift3 changeWinSz windowSz (lift0 ((-1)*percent)) (lift0 maxWindowSize)) 
						(windowSz))
		    in
		        tuple5Z newX newY (myWatch 1 "Window: " newWinSz) newnoBlobCount newBlobCount)

		ulx=lift1 toDouble $ ((sel5_1Z newXY)-((sel5_3Z newXY)/2))
		uly=lift1 toDouble $ ((sel5_2Z newXY)-((sel5_3Z newXY)/2))
		lrx=lift1 toDouble $ ((sel5_1Z newXY)+((sel5_3Z newXY)/2))
		lry=lift1 toDouble $ ((sel5_2Z newXY)+((sel5_3Z newXY)/2))
		
	    in
	        (dotAt (lift0 Red)
		      (point2XY (lift1 toDouble (sel5_1Z newXY)) 
				(lift1 toDouble (sel5_2Z newXY)))) 
		`over` (forceIO (rectangle 
			(point2XY ulx uly) (point2XY lrx lry)))
	)

main = play (followMe)
