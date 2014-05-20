module As1 where

import Frob
import RobotSim

as1RobotModel :: SimbotModel
as1RobotModel d v_max a_max aa_max v_l0 v_r0 =
	let
		v_0 = (v_l0 + v_r0) / 2
		v_rd = limitSymB v_max desiredVRightB
		v_ld = limitSymB v_max desiredVLeftB
		a = lift1 signum (v_rd+v_ld-2*v) * lift0 a_max
		v = lift0 v_0 + integralB a
		v_diff = (v_rd - v_ld) / 2
		v_r = v + av
		v_l = v - av

		-- Lines Added
		w_0 = (v_r0 - v_l0) / d
		aa = lift1 signum (v_rd-v_ld- lift0 d*w) * lift0 aa_max
		w = lift0 w_0 + integralB aa
		av = lift0 d * w /2
	in
		withVelocities v_l v_r
