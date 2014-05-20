
-- Part of the FVision core.  Used to draw graphic overlays on the
-- animation window.

module Draw where

import qualified SOEGraphics as G 
import SOEGraphics(Color(..))
import Graphics(Picture(..))
import GeometryStatic

-- Should move the Picture type over here from Graphics and add ImageRGB to the
-- type.   -- jcp


drawPic                 :: Picture -> G.Graphic
drawPic p = G.withColor White $
            drawp p (uscale2 1) -- Use pixel coordinates
 where 
  drawp (p1 `OverPic` p2) t = drawp p1 t `G.overGraphic` drawp p2 t

  drawp EmptyPic _          = G.emptyGraphic

  drawp (LinePic p1 p2) t   = G.line (t %$. p1) (t %$. p2) 

  drawp (ArcPic p1 p2 a1 a2) t = G.arc (t %$. p1) (t %$. p2)
                                        (radianToDegree a1)
                                        (radianToDegree a2)

  drawp CirclePic t         = G.ellipse (t %$. point2XY (-1) (-1)) 
	                                (t %$. point2XY 1 1) 

  drawp (TransformedPic t' p) t = drawp p (t `compose2` t') 
 
  drawp (ColoredPic c p) t = G.withColor c $ drawp p t
 
  drawp (TextPic s) t = G.text (t %$. point2XY 0 0) s  
 
  drawp (PolygonPic ps) t = G.polygon (map (\x -> t %$. x) ps) 

  drawp (PolylinePic (p:ps)) t = G.polyline (map (\x -> t %$. x) ((p:ps)++[p]))

(%$.) :: Transform2 -> Point2 -> (Int, Int) 
t %$. p = let Point2XY x y = t %$ p in 
   (round x, round y) 
