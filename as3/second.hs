module WallFollower where

import Frob
import RobotSim
--import MyRobotModel

emptyWorld    = maybeReadWorldFromPath "as33"

maybeReadWorldFromPath p = do
   w <- readWorldFromPath p
   case w of
      Right w' -> return w'
      Left s   -> return []

wheelSpeedsT :: WheelControl o => Behavior i Speed -> Behavior i Speed -> RobotTask s i o e
wheelSpeedsT vl vr = liftT $ setWheelSpeeds (pairZ vl vr)

vAndTurningRateT :: (RobotProperties i, WheelControl o) =>
			Behavior i Speed -> Behavior i AngSpeed -> RobotTask s i o e
vAndTurningRateT v omega = liftT $
		setWheelSpeeds (pairZ (v-dv) (v+dv)) where
			dv = robotDiameter * omega / 2

turnToT :: (Odometry i, WheelControl o) =>
		Angle -> RobotTask s i o ()
turnToT ang = 
		wheelSpeedsT (-vDiff) vDiff `tillT_`
			isTrueE (abs (lift1 normalizeHeading diffHeading) <=* 0.1)
		where diffHeading = lift0 ang - heading
		      vDiff = 0.1

turnRelT :: (Odometry i, WheelControl o) =>
		Angle -> RobotTask s i o ()
turnRelT relativeAng =
		do initHeading <- snapshotNowT heading
		   turnToT (initHeading + relativeAng)

-- keep on going to the specified point
gotoPointT :: (RobotProperties i, Odometry i, WheelControl o) =>
           Behavior i Point2 -> RobotTask s i o e
gotoPointT goal =
    vAndTurningRateT 0.3 (lift1 normalizeHeading $ goalDir - heading) where
        goalDir = (sndZ . vector2PolarCoords) $ normalize (goal .-. position)

-- go to the specified point and stop
gotoPointAndStopT :: (RobotProperties i, Odometry i, WheelControl o) =>
           Behavior i Point2 -> RobotTask s i o ()
gotoPointAndStopT goal =
    gotoPointT goal `tillT_`
        (isTrueE $ magnitude (goal .-. position) <* 0.2)

followWallonLeftT :: (RobotProperties i, RangeDetection i, WheelControl o) =>
			Length -> RobotTask s i o e
followWallonLeftT d = vAndTurningRateT v (0.2 * (signum diffD))
		      where
			v = 0.24
			leftD = lift1 (!! 1) rangeReadings
			diffD =  (leftD - lift0 d)
followWallT :: (RobotProperties i, Odometry i, RangeDetection i, StuckDetection i, WheelControl o) =>
			Length -> RobotTask s i o e
followWallT d = foreverT $ do sendT "Following Left Wall...\n"
			      followWallonLeftT d `tillT_` 
			       isTrueE ((abs leftD >* 3) ||* (frontDiffD <* 0.1))
			      p0 <- snapshotNowT (frontDiffD <* 0.1)
			      let omega = if (not p0) then (0.3) else (-0.3)
			      initHeading <- snapshotNowT heading
			      sendT "Turning...\n"
			      vAndTurningRateT 0 omega `tillT_`
		--	       isTrueE (abs(abs (heading - lift0 initHeading) - pi/2)<* 0.1)
			       isTrueE (abs leftD <* 3 &&* frontDiffD >* 0.1)
			       where
			       	  leftD = lift1 (!! 1) rangeReadings
				  leftDiffD = (leftD - lift0 d)
				  frontD = lift1 (!! 0) rangeReadings
				  frontDiffD = (frontD - lift0 d)


main = do w <- emptyWorld
	  runSimRunT w () $ do  followWallT 1
	  		       
				
