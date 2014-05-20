module Test( main ) where

import qualified NXVision2 as XV
import XVWindow
import GraphicsCore 
import GraphicsUtils
import IOExts( IORef, newIORef, readIORef, writeIORef )
--import qualified Draw as D
--import qualified Graphics as G
--import qualified GeometryStatic as S

-- This plays a MPEG

main :: IO ()
main = do
  grabber <- XV.openMPEG "ball.mpg"
  putStrLn "Opened test.mpg"
  window  <- openXVWindow grabber 100 100
  putStrLn "Opened window at 100 100"
  let
    loop pos@(x,y) = do
      img <- XV.nextImage grabber
      putStrLn "Acquired image"
      drawImageOnXVWindow window img
      drawOnXVWindow window $ circle pos 10
--       D.drawPic (S.moveXY (fromInt x) (fromInt y) $
--                  S.stretch 10 $
--                  G.withColor G.Red $
--                  G.circle)
      flushXVWindow window
      es <- getXVWindowEvents window
--      print es      
      loop (updateMouse es pos)
  loop (0,0)

-- find the last mouse move event
-- could be written more cryptically/elegantly with foldl
updateMouse :: [Event] -> Point -> Point
updateMouse []                     pt = pt
updateMouse (MouseMove{pt=pos}:es) pt = updateMouse es pos
updateMouse (_:es)                 pt = updateMouse es pt

circle (x,y) r = ellipse (x-r,y-r) (x+r,y+r)

-- Test imageSize

main1 :: IO ()
main1 = do
  grabber <- XV.openMPEG "test.mpg"
  window  <- openXVWindow grabber 0 0
  let
    loop = do
      img <- XV.nextImage grabber
      drawImageOnXVWindow window img
      flushXVWindow window
      es <- getXVWindowEvents window
      let (rows, cols) = XV.imageSize img
      putStrLn ("Rows = " ++ show rows ++ " Cols = " ++ show cols)
--      print es      
      loop
  loop

-- Test getRegion :: ImageRGB -> Int -> Int -> Int -> Int -> ImageRGB
-- Shows acquired region only

main2 :: IO ()
main2 = do
  grabber <- XV.openMPEG "test.mpg"
  window  <- openXVWindow grabber 0 0
  let
    loop = do
      img <- XV.nextImage grabber
      let img2 = XV.getRegion img 100 100 100 100
      drawImageOnXVWindow window img2
      flushXVWindow window
      es <- getXVWindowEvents window
      loop
  loop

main3 :: IO ()
main3 = do
  grabber <- XV.openMPEG "test.mpg"
  window  <- openXVWindow grabber 50 50 
  window2 <- openXVWindow grabber 500 50
  let
    loop angle = do
      img <- XV.nextImage grabber
      drawImageOnXVWindow window img
      flushXVWindow window
      let img2 = XV.getRegionRotated img 120 120 100 100 angle
      drawImageOnXVWindow window2 img2
      flushXVWindow window2
      es <- getXVWindowEvents window
      loop (angle + (pi / 180) * 2)
  loop 0

main4 :: IO ()
main4 = do
  grabber <- XV.openMPEG "test.mpg"
  window  <- openXVWindow grabber 50 50 
  window2 <- openXVWindow grabber 500 50
  img <- XV.nextImage grabber
  drawImageOnXVWindow window img
  flushXVWindow window
  let img2 = XV.getRegion img 100 100 50 50
  drawImageOnXVWindow window2 img2
  flushXVWindow window2
  es <- getXVWindowEvents window
  flushXVWindow window2
  getLine
  let c = XV.getColorSelector img2
  c `seq` return ()
  putStrLn "Looping ..."
  let
    loop (x, y) = do
      img <- XV.nextImage grabber
      drawImageOnXVWindow window img
      flushXVWindow window
      let img2 = XV.getRegion img (x-30) (y-30) 60 60
      drawImageOnXVWindow window2 img2
      flushXVWindow window2
      let (xc, yc, area) = XV.stepBlob img2 c
      putStrLn ("x = " ++ show xc ++ " y = " ++ show yc ++ " area = " ++ show area)
      let dx = (round xc - 30) :: Int
          dy = (round yc - 30) :: Int
      es <- getXVWindowEvents window
      loop (x+dx, y+dy)
  loop (130, 130) 







