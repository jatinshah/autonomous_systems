
-- This is the old SSD tracker.  It should not be needed anymore.
-- Obsolete file.

module SSD
	( module SSD
	) where

import XVTypes
import Pipe
import Matrix
import Image
import Geometry

import IOExts
import Monad
import Maybe
import List( transpose )
import qualified AnsiScreen
import Init
import Array

-- ToDo: rename this sucker
mkSSD :: ImageInt -> Matrix
mkSSD ref0 = m
 where
  sz = sizeOf ref0 - 1
  dx = smoothDxImageInt ref0
  dy = smoothDyImageInt ref0
  dt = dx `productImage` cY sz - dy `productImage` cX sz
  m  = colsToMatrix (truncate (areaOf sz)) [dx,dy,dt] 

-- ToDo: add enough colVector/rowVector support to let me write:
-- cX (c,r) = column [-hc..hc]       * row (replicate r 1)
-- cY (c,r) = column (replicate c 1) * column [-hr..hr]

cY :: Size -> Matrix
cY sz@(Size c r) = listToMatrixSz sz (replicate (round r) [-hc..])
 where
  hc = fromInteger $ truncate $ ((c-1) / 2)

cX :: Size -> Matrix
cX sz = transposeMatrix (cY (transposeSize sz))


--- The SSD stepper

ssdStep :: ImageInt -> ImageInt -> (Transform2, Error)
ssdStep r0 = \ r -> 
           let
             error     = compressxyImageInt (r0 - r)
             delta     = m0 `multColVector` imageToColVector error
             residuals = imageToMatrix error - m * delta
             residual  = normMatrix residuals
           in
           ( mkTransform (head (matrixToList delta)), residual )
 where
  m   = mkSSD r0
  m0  = inverseMatrix (m_t * m) * m_t
  m_t = transposeMatrix m

  mkTransform :: [Double] -> Transform2
  mkTransform [x,y,theta] = rotate2 theta `compose2` translate2 (vector2XY x y)

