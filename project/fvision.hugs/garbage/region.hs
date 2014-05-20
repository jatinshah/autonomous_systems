
-- I think this file is obsolete since it uses Overload -- jcp.  

----------------------------------------------------------------
-- Region tracker
----------------------------------------------------------------

module Region
	( SSDTracker, make, update
	, tracker
	) where

import Video
	( Video
	, acquireRegion
	)
import XVision
	( EdgeTracker, mkEdgeTracker, find_EdgeTracker
	, colorToBW
	)
import SP
	( Tracker, statefulSP
	)
import Error
	( Error
	)
import Geometry

import Pipe
import IOExts
import XVision hiding (Offset)
import Prelude hiding(sum,or)
import XVUtilities
import Overload(size)

----------------------------------------------------------------
-- Exports
--
-- One of the interesting things in this interface is that 
-- the SSD tracker always scales its input image to match
-- the original image.  So, if you're tracking a 100x200
-- reference image and the Region shrinks to 50x100 then
-- the image is doubled in size while being grabbed.
----------------------------------------------------------------

make    :: Image_Int -> IO SSDTracker
update  :: Video -> SSDTracker -> Region -> IO (Region,Error)
tracker :: Video -> Image_Int -> Tracker Region

----------------------------------------------------------------
-- Implementations
----------------------------------------------------------------

type SSDTracker = (Sz,Image_Int -> (DeltaRegion,Error))

make im = return (size im, ssd im)

update v (sz@(w,h),ssd) r = do
  { pic <- acquireRegion v sz r
  ; let (rawdelta,residual) = ssd (colorToBW pic)
  ; let delta = rotate' sx sy a rawdelta
  ; return (r `addRegionDelta` delta, residual)
  }
 where
  (sz_x,sz_y) = size_Region r
  sx = fromInt w / sz_x
  sy = fromInt h / sz_y
  a = angle_Region r
  addRegionDelta = undefined

bigger :: Double -> Sz -> Sz
bigger n (x,y) = (round (n * fromInt x), round (n * fromInt y))

-- ToDo: I doubt I'm doing the right things with sx and sy
rotate' sx sy theta (dx,dy,dtheta) = (sx * dx',sy * dy',dtheta)
 where
  Cartesian dx' dy' = rotate (-theta) (Cartesian dx dy)

tracker v im = statefulSP (make im) (update v)

----------------------------------------------------------------
-- SSD
--
-- This is the core of the original XVision SSD tracker
--
-- It could do with a cleanup to match the new interface.
----------------------------------------------------------------

type DeltaRegion = (Double,Double,Double)

ssd :: Image_Int -> (Image_Int -> (DeltaRegion,Error))
ssd r0 = \ r -> 
           let
             error     = compressxy (r0 - r)
             delta     = m0 `multColVector` imageToColVector error
             residuals = imageToMatrix error - m * delta
             residual  = norm residuals
           in
           ( mkDelta (head (fromMatrix delta)), residual )
 where
  m   = mkSSD r0
  m0  = inverse_M (m_t * m) * m_t
  m_t = transpose_M m

  mkDelta :: [Double] -> DeltaRegion
  mkDelta [x,y,theta] = (x,y,theta)

-- ToDo: rename this sucker
mkSSD :: Image_Int -> Matrix
mkSSD ref0 = m
 where
  (x,y) = size ref0 - (1,1)
  dx = smoothDx ref0
  dy = smoothDy ref0
  dt = dx `productImage` cY (x,y) - dy `productImage` cX (x,y)
  m  = colsToMatrix (x*y) [dx,dy,dt] 

-- ToDo: add enough colVector/rowVector support to let me write:
-- cX (c,r) = column [-hc..hc]       * row (replicate r 1)
-- cY (c,r) = column (replicate c 1) * column [-hr..hr]

cY :: Sz -> Matrix
cY (c,r) = matrix (c,r) (replicate r [-hc..])
 where
  hc = fromInt ((c-1) `div` 2)

cX :: Sz -> Matrix
cX (c,r) = transpose_M (cY (r,c))

----------------------------------------------------------------
-- Matrix support
----------------------------------------------------------------

matrix :: (Int,Int) -> [[Double]] -> Matrix
matrix (r,c) xs = unsafePerformIO $
  do{ m <- emptyMatrix (c,r)
    ; mapM_ (setMatrix m) (zip ixs (concat (map (take r) xs)))
    ; return m
    }
 where
  ixs = [0..c-1] `cross` [0..r-1]

fromMatrix :: Matrix -> [[Double]]
fromMatrix m = [ [ getMatrix m (x,y) | y <- [0..r-1] ] | x <- [0..c-1] ]
 where
  (c,r) = size m

mtMatrix :: Sz -> Matrix
mtMatrix sz = unsafePerformIO (emptyMatrix sz)

colsToMatrix :: Int -> [Image_Int] -> Matrix
colsToMatrix sz is = foldl addColumn2 (mtMatrix (0,sz)) is

cross :: [a] -> [b] -> [(a,b)]
cross as bs = [(a,b) | a <- as, b <- bs]

----------------------------------------------------------------
-- Static Image ops
----------------------------------------------------------------

compressxy :: Image_Int -> Image_Int
compressxy = compressx . compressy

----------------------------------------------------------------
-- End
----------------------------------------------------------------
