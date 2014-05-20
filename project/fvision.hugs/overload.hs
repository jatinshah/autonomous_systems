----------------------------------------------------------------
-- Overloading
----------------------------------------------------------------

module Overload
	( 
	) where

-- Obsolete now.  Use Geometry and Matrix

import Prelude hiding(sum)
import XVision

class    Size a           where
   size :: a -> Size2

instance Size Image_Int   where size = size_Image_Int
instance Size Image_RGB   where size = size_Image_RGB
instance Size Image_Float where size = size_Image_Float
instance Size Video       where size = sizeVideo
instance Size Mpeg        where size = sizeMPEG
instance Size Matrix      where size = size_M

instance Eq   Image_Int	-- bogus
instance Show Image_Int	-- bogus
instance Num  Image_Int	where
  (+) = plusImage
  (-) = minusImage

instance Eq   Matrix	-- bogus
instance Show Matrix	-- bogus
instance Num  Matrix	where
  (+) = addMatrix
  (-) = minusMatrix
  (*) = multMatrix

----------------------------------------------------------------
-- End
----------------------------------------------------------------
