module WallFollower where

import Frob
import RobotSim
import RobotModel

emptyWorld    = maybeReadWorldFromPath "as31"

maybeReadWorldFromPath p = do
   w <- readWorldFromPath p
   case w of
      Right w' -> return w'
      Left s   -> return []

vAndTurningRateT :: (RobotProperties i, WheelControl o) =>
			Behavior i Speed -> Behavior i AngSpeed -> RobotTask s i o e
vAndTurningRateT v omega = liftT $
		 setWheelSpeeds (pairZ (v-dv) (v+dv)) where
			dv = robotDiameter * omega / 2

gain1 = (-0.1)	-- Gain of the error
gain2 = (-1.0)	-- Gain of derivative of the error

followWallT :: (RobotProperties i, RangeDetection i, WheelControl o) =>
			Length -> RobotTask s i o e
followWallT d = vAndTurningRateT v (lift0 gain1 * diffD + lift0 gain2 * ddiffD)
		      where
			v = 0.4
			leftD = lift1 (!! 1) rangeReadings
			diffD =  (lift0 d - leftD)
			ddiffD = derivativeB diffD


main = do w <- emptyWorld
	  runSimRunT w () $ do followWallT 2
	  		       
				
