
-- This is OLD CODE.  IT DOES NOT WORK!!!

-- General image stuff; mostly imported from C++


module Image( maskToImage          -- Mask -> ImageInt
            , imageToMask          -- ImageInt -> Mask
            , emptyImageInt        -- ISize -> ImageInt
            , iPlusImageInt        -- Int -> ImageInt -> ImageInt
            , iDivImageInt         -- Int -> ImageInt -> ImageInt
            , iTimesImageInt       -- Int -> ImageInt -> ImageInt
            , innerProductImageInt -- ImageInt -> ImageInt -> Double
            , sumImageInt          -- ImageInt -> Double
            , subImageInt          -- (Int,Int) -> (Int,Int) -> ImageInt ->
                                   --                           ImageInt
            , scaleImageInt        -- (Double,Double) -> ImageInt -> ImageInt
            , reduceResolutionImageInt  -- (Int,Int) -> ImageInt -> ImageInt
            , magnifyImageInt       -- (Int,Int) -> ImageInt -> ImageInt
            , convolveImageInt      -- Mask -> ImageInt -> ImageInt
            , product
            , gaussianMask          -- (Int,Int) -> Double -> Mask
            , dxImageInt            -- ImageInt -> ImageInt
            , dyImageInt            -- ImageInt -> ImageInt
            , smoothDxImageInt      -- ImageInt -> ImageInt
            , smoothDyImageInt      -- ImageInt -> ImageInt
            , compressxImageInt     -- ImageInt -> ImageInt
            , compressyImageInt     -- ImageInt -> ImageInt
            , compressxyImageInt    -- ImageInt -> ImageInt
            , colorToBWImage        -- ImageRGB -> ImageInt
            , bwToColorImage        -- ImageInt -> ImageRGB
            , avgColorImageRGB      -- RGBtriple -> ImageRGB -> ImageInt
            , mkImageInt            -- ISize -> Int -> ImageInt
            , threshBWImageInt      -- Int -> Int -> ImageInt -> ImageInt
            , centroidP2BWImageInt    -- ImageInt -> (Point2,Double)
            ) where

-- operations on image types

import XVTypes
import XVision
import GeometryStatic

import FVUtils


---------------
-- ImageInt (as a matrix of Ints)
---------------

instance Eq   ImageInt	-- bogus; could be fixed!

instance Show ImageInt	where
  showsPrec p i = showSized "ImageInt" i

instance Num  ImageInt	where
  (+)    = plusImageInt
  (-)    = minusImageInt
  (*)    = multImageInt
  abs    = absImageInt
  negate = iTimesImageInt (-1)

-- could also add VectorSpace except you need to round

instance Sized ImageInt   where sizeOf = toSize . sizeImageInt
instance Sized ImageRGB   where sizeOf = toSize . sizeImageRGB
instance Sized ImageFloat where sizeOf = toSize . sizeImageFloat

compressxyImageInt = compressxImageInt . compressyImageInt

centroidP2BWImageInt:: ImageInt -> (Point2, RealVal)
centroidP2BWImageInt im = (Point2XY x y,v)
				where 
				  ((x,y),v) = centroidBWImageInt im
                                  

