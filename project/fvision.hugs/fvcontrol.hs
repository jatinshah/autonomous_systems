
-- This defined FVision controllers.  Part of the FVision core.


module FVControl where

import Animate
import FRP
import Graphics

showPic :: Behavior VideoInput Picture -> Behavior VideoInput VideoOutput
showPic = lift1 (\pic -> VideoOutput {voOverlay = emptyPic, voPicture = pic, voMessage = "" })


addMessage :: Behavior VideoInput String -> Behavior VideoInput (VideoOutput -> VideoOutput)
addMessage = lift1 (\msg vo -> vo {voMessage = msg ++ voMessage vo})



