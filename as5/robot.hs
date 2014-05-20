module GoalSeeker where

import Frob
import RobotSim
import qualified GeometryStatic as S
import Trace
import FRPDebugUtils

-- Gain constants used in the controllers.
-- You can tweak kpDist and kpOrtn, but should not touch kiDist and kiOrtn, 
-- which are changed automatically accordingly.

kpDist :: FRPReal -- proportional gain in distance control
kpDist  = 1
kpOrtn :: FRPReal -- proportional gain in orientation control
kpOrtn  = 1

-- The following definitions yield critical damping; don't change them!

kiDist :: FRPReal -- integral gain in distance control
kiDist  = kpDist*kpDist/4
kiOrtn :: FRPReal -- integral gain in orientation control
kiOrtn  = kpOrtn*kpOrtn/4

-- Force Coefficients
attrC :: FRPReal
attrC = 0.1
replC :: FRPReal
replC = 10
wreplC :: FRPReal
wreplC = 0.7

-- Robot Model
-- The model of a Simbot that has neither translational nor rotational inertia
noInertiaSimbotModel :: SimbotModel
noInertiaSimbotModel d v_max a_max acc_max v_l0 v_r0 =
    let
        v_r  = desiredVRightB
        v_l  = desiredVLeftB
    in
        withVelocities v_l v_r

-- Controllers
-- The controller that sets the translational speed and the rotational speed
forwardRotationC :: (RobotProperties i, WheelControl o) =>
     Behavior i Speed     -- the translational speed
     -> Behavior i AngSpeed  -- the rotational speed
     -> RController i o
forwardRotationC spd omega =
     setWheelSpeeds (pairZ (spd - dSpd) (spd + dSpd)) where
               dSpd   = robotDiameter * omega / 2

-- The controller that sets the velocity (a vector) of the robot.
-- This is a PI controler that achieves critical damping.
velocityC :: (RobotProperties i, Odometry i, WheelControl o) =>
             Behavior i Vector2   -- the velocity vector
	  -> RController i o
velocityC vel = let (spd, ang)  = vector2PolarCoordsTuple vel
	            dAng       = lift1 normalizeHeading $ ang - heading
	            omega      = lift1 (kpOrtn *) dAng + lift1 (kiOrtn *) (integralB dAng)
	        in  forwardRotationC spd omega

obstacleC :: (GlobalWorldPerception i,RobotProperties i, Odometry i, WheelControl o) => RController i o
obstacleC       = let
			obstacles = gwpObstacles
			robots = gwpRobots
			otherRobots = lift2 select robotId robots
			goal = point2XY 3 3
			myPosition = lift1 getMaybe $ lift2 lookup robotId robots
			goalDistance = goal .-. myPosition
			myMap = lift2 (\x y -> map ((S..-.) x) y)
			myFoldl :: Behavior i Vector2 -> Behavior i [Vector2] -> Behavior i Vector2
			myFoldl = lift2 $ foldr (S.^+^) 
			obstacleDistance = myFoldl zeroVector $ myMap myPosition obstacles
			robotDistance = myFoldl zeroVector $ myMap myPosition otherRobots
			-- Repulsion from the walls
			-- Very primitive
			myPositionX = point2XCoord myPosition
			myPositionY = point2YCoord myPosition
			wallForceX = 1/(4.8+myPositionX)^2-1/(4.8-myPositionX)^2
			wallForceY = 1/(4.8+myPositionY)^2-1/(4.8-myPositionY)^2
			wallRepulsion = lift0 wreplC *^ vector2XY wallForceX wallForceY
			-- Force = const. * distance   for attraction
			-- Force = const. / distance   for repulsion
			attraction = lift0 attrC *^ goalDistance
			repulsiveDistance = obstacleDistance ^+^ robotDistance
			repulsiveMagnitude = magnitudeSquared repulsiveDistance
			repulsion = (lift0 replC / repulsiveMagnitude) *^ (obstacleDistance ^+^ robotDistance)
			v = attraction ^+^ repulsion ^+^ (watchB (\t a -> show a ++ "\n") wallRepulsion)
		  in
		  	velocityC v
main = do w <- maybeReadWorldFromPath "as5"
          runSimRunEx1 w noInertiaSimbotModel obstacleC 

-- Miscellaneous Functions
getMaybe a = b where Just b = a
select robotId [] = []
select robotId (x:xs) = if fst x /= robotId then (snd x:select robotId xs)
			else select robotId xs
