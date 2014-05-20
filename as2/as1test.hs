
import Frob
import RobotSim
import As1

-- control the robot by specifying the translational velocity and the
-- rotational velocity (turning rate)
vAndTurningRateC :: (RobotProperties i, WheelControl o) =>
		    Behavior i FRPReal -> Behavior i FRPReal -> RController i o
vAndTurningRateC v omega =
    setWheelSpeeds (pairZ (v - dv_2) (v + dv_2)) where
        dv   = robotDiameter * omega
	dv_2 = dv/2
        d    = 1

-- keep on going to the specified point
gotoPointC :: (RobotProperties i, Odometry i, WheelControl o) =>
	   Behavior i Point2 -> RController i o
gotoPointC goal = 
    vAndTurningRateC 0.3 (goalDir - heading) where
        goalDir = (sndZ . vector2PolarCoords) $ normalize (goal .-. position)


main = do w <- readWorldFromPath "empty1"
	  runSimRunEx1 w as1RobotModel (gotoPointC $ point2XY (-4) 3)

