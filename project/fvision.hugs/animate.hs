{-
******************************************************************************
*                              FVISION                                       *
*                                                                            *
*       Module:         Animate                                              *
*       Purpose:        Behaviour animation.                                 *
*                                                                            *
******************************************************************************
-}

module Animate where

import IOExts (IORef, newIORef, readIORef, writeIORef)

import FRP
import FRPSignal(normalizeE)
import GeometryStatic
import Graphics        -- in frp/src
import Force           -- in frp/src
import qualified GraphicsEvent as G
import qualified NXVision2 as XV
import NXVision2(ImageRGB)
import XVWindow    -- Alastair's XV window code
--import FXVision2   -- The fake module
import GraphicsKey
import qualified Draw as D
------------------------------------------------------------------------------
-- Exported entities
------------------------------------------------------------------------------

data VideoInput = VideoInput {
                    vilbp   :: ! Maybe (),
                    virbp   :: ! Maybe (),
                    viMouse :: ! Point2,
                    viKey   :: ! Maybe Char,
                    viImage :: ! ImageRGB }

instance Show VideoInput where
  showsPrec _ v = showString ("<<Frame>>")

instance Forceable VideoInput where
    force = id

instance ConsoleInput VideoInput where
   keyE   = behaviorToEvent $ (lift1 viKey) inputB

instance GuiInput VideoInput where
   mmE    = normalizeE $ behaviorToEvent $ (lift1 (\vi -> Just (viMouse vi))) inputB
   mouseB = lift1 viMouse inputB
   lbpE   = normalizeE $ behaviorToEvent $ lift1 vilbp inputB
   rbpE   = normalizeE $ behaviorToEvent $ lift1 virbp inputB

videoB :: Behavior VideoInput ImageRGB
videoB = lift1 viImage inputB

data VideoOutput = VideoOutput {
                    voOverlay :: Picture,
                    voPicture :: Picture,
                    voMessage :: String }

instance Show VideoOutput where
  showsPrec _ _ = showString "<<video output>>"

instance Forceable VideoOutput where
 force vo@(VideoOutput {voMessage = msg}) =
        msg `seq` vo

-- Animate a behaviour.
-- file  ...... Name of the mpeg file
-- outputDir .. 
-- b .......... Behavior to animate.

runmpeg :: String -> String ->
           (Behavior VideoInput VideoOutput) ->
           IO ()
runmpeg mpegFile outputDir b = runGraphics $
    do putStrLn ("Running FVision on " ++ mpegFile)
       if outputDir /= ""
         then putStrLn ("Saving mpeg into directory " ++ outputDir)
         else return ()
       mpeg <- if mpegFile=="bttv" then XV.openBTTV ""
               else if mpegFile=="firewire" then XV.openFireWire ""
               else XV.openVideo mpegFile
       xvWindow <- openXVWindow mpeg 0 0  -- The mpeg sizes the window
       mpegFrameRate <- return 20.0 -- Should come from the mpeg file?
       -- Do this in XVision?
       lastMouse <- newIORef origin2
       lastTime <- newIORef 0
       theImage <- newIORef (error "bad stuff in runmpeg!")
       theEvents <- newIORef []
       let getVisionInput = 
             do t <- readIORef lastTime   -- Increment the time base
                t `seq` writeIORef lastTime (t + 1/mpegFrameRate)
                image <- XV.nextImage mpeg
                writeIORef theImage image
                m <- readIORef lastMouse
                let vi = initVideoInput image m
                es <- readIORef theEvents
                let vi' = addInputEvents es vi
                writeIORef lastMouse (viMouse vi')
                return (Just (t, vi'))
       let showVisionOutput vo = 
             do if voMessage vo /= "" 
                    then putStrLn (voMessage vo)
                    else return ()
                image <- readIORef theImage
                force (voPicture vo) `seq`  -- Make sure all observation is complete
                  force (voOverlay vo) `seq`
                    (do drawImageOnXVWindow xvWindow image
                        drawOnXVWindow xvWindow (D.drawPic (voPicture vo))
                        flushXVWindow xvWindow
                        es <- getXVWindowEvents xvWindow  -- Can't go in getVision!!!
                        writeIORef theEvents es)

       reactimateB getVisionInput showVisionOutput b


-- reactimateB :: IO (Maybe (Time, inp)) ->  -- getTime
--               (b -> IO ())   ->  -- render
--               Behavior inp b ->  -- behavior
--               IO ()
-- Get window events, with "redundant" mouse moves removed.

initVideoInput i m = VideoInput {vilbp = Nothing,
                                 virbp = Nothing,
                                 viMouse = m,
                                 viKey = Nothing,
                                 viImage = i }

addInputEvent :: G.Event -> VideoInput -> VideoInput
addInputEvent e i =
  case e of
    G.Key {G.key = key, G.isDown = True} | isCharKey key ->
        i {viKey = Just (keyToChar key)}
    G.Button {G.pt = pt, G.isDown = True, G.isLeft = True}  -> 
        i {vilbp = Just (), viMouse = toPoint2 pt}
    G.Button {G.pt = pt, G.isDown = True, G.isLeft = False} -> 
        i {virbp = Just (), viMouse = toPoint2 pt}
    G.MouseMove {G.pt = pt} -> i {viMouse = toPoint2 pt}
    _ -> i                  
 where toPoint2 (x,y) = point2XY (fromInteger (toInteger x)) (fromInteger (toInteger y))

addInputEvents :: [G.Event] -> VideoInput -> VideoInput
addInputEvents [] i = i
addInputEvents (e:es) i = addInputEvents es (addInputEvent e i)
