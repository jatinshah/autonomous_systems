module RobotModel where

import Frob
import RobotSim

RobotModel :: SimbotModel
RobotModel d v_max a_max aa_max v_l0 v_r0 =
	let
		v_rd = limitSymB v_max desiredVRightB
		v_ld = limitSymB v_max desiredVLeftB
	in
		withVelocities v_ld v_rd
