
-- This file has not been adapted to the new .gc file yet.
-- OLD CODE!  Does not work yet.

-- Warning!!  Always use sizes that are width, height.   Matrix indexing
-- is always with the column first: (c,r).  When a matrix is printed, it
-- prints width (# of columns) first.

module Matrix( Matrix
             , emptyMatrix        --  Sz -> IO Matrix
             , setMatrix          --  Matrix -> ((Int,Int),Double) -> IO ()
             , getMatrix          --  Matrix -> (Int,Int) -> Double
             , productImage       --  ImageInt -> Matrix -> ImageInt
             , addColumn          --  Matrix -> ImageInt -> Matrix
             , addColumn2         --  Matrix -> ImageInt -> Matrix
             , imageToMatrix      --  ImageInt -> Matrix
             , matrixToImage      --  Matrix -> ImageInt
             , imageToColVector   --  ImageInt -> ColVector
             , inverseMatrix      --  Matrix -> Matrix
             , transposeMatrix    --  Matrix -> Matrix
             , innerProductMatrix --  Matrix -> Matrix
             , outerProductMatrix --  Matrix -> Matrix
             , normMatrix         --  Matrix -> RealVal 
             , arrayToMatrix      --  Array Int Double -> Matrix
             , matrixToList       --  Matrix -> [[Double]]
             , listToMatrix       --  [[Double]] -> Matrix
             , listToMatrixSz     --  Size -> [[Double]] -> Matrix   
             , colsToMatrix       --  Int -> [ImageInt] -> Matrix
             , multColVector      --  Matrix -> ColVector -> Matrix

    ) where

-- Matrix operatations (including column vectors)

import Array
import IOExts(unsafePerformIO)
import XVTypes

import XVision( emptyMatrix
              , setMatrix     --  Matrix -> ((Int,Int),Double) -> IO ()
              , getMatrix     --  Matrix -> (Int,Int) -> Double
              , productMatrix --  Matrix -> Matrix -> Matrix
              , productImage  --  ImageInt -> Matrix -> ImageInt
              , sizeMatrix    --  Matrix -> Sz
              , addColumn 
              , addColumn2    --  Matrix -> ImageInt -> Matrix
              , imageToMatrix --  ImageInt -> Matrix
              , matrixToImage --  Matrix -> ImageInt
              , multMatrix    --  Matrix -> Matrix -> Matrix
              , addMatrix     --  Matrix -> Matrix -> Matrix
              , minusMatrix   --  Matrix -> Matrix -> Matrix
              , inverseMatrix --  Matrix -> Matrix
              , transposeMatrix --  Matrix -> Matrix
              , innerProductMatrix --  Matrix -> Matrix
              , outerProductMatrix --  Matrix -> Matrix
              , normMatrix    --  Matrix -> Double
              , imageToColVector --  ImageInt -> ColVector
              , multColVector --  Matrix -> ColVector -> Matrix
              )

import Geometry

-- Warning: array must have 0 as lower bound cos I'm too lazy to do
-- this the right way.
-- test: let m = mkMatrix (listArray ((0,0),(3,3)) [1..]) in size_M m

-- I think this violates the r,c convention!

arrayToMatrix :: Array (Int,Int) Double -> Matrix
arrayToMatrix xs = unsafePerformIO $
  do{ m <- emptyMatrix (c,r)
    ; mapM_ (setMatrix m) (assocs xs)
    ; return m
    }
 where
  ((rl,cl),(rh,ch)) = bounds xs
  r = 1 + rh - rl
  c = 1 + ch - cl

instance Eq   Matrix	-- bogus but could be done

instance Show Matrix where
  showsPrec p = showSized "Matrix"

instance Num  Matrix	where
  (+) = addMatrix
  (-) = minusMatrix
  (*) = multMatrix
  -- ought to add negate

instance Sized Matrix     where sizeOf = toSize . sizeMatrix

-- Use the length of the first row to set number of columns.  Excess / 
-- missing values ignored.

listToMatrix :: [[Double]] -> Matrix  -- Input is a list of rows
listToMatrix xs = unsafePerformIO $
  do m <- emptyMatrix (cols,rows)
     mapM_ (setMatrix m)
           [((c,r),v) | (r,row) <- zip [0..rows-1] xs,
                        (c,v) <- zip [0..cols-1] row]

     return m
 where
  rows = length xs
  cols = if rows == 0 then 0 else length (head xs)

listToMatrixSz :: Size -> [[Double]] -> Matrix
listToMatrixSz sz xs = unsafePerformIO $
  do m <- emptyMatrix (cols,rows)
     mapM_ (setMatrix m)
           [((c,r),v) | (r,row) <- zip [0..rows-1] xs,
                        (c,v) <- zip [0..cols-1] row]

     return m
 where
   (cols,rows) = toISize sz


matrixToList :: Matrix -> [[Double]] 
matrixToList m = [ [ getMatrix m (x,y) | x <- [0..c-1] ] | y <- [0..r-1 ] ]
 where
  (c,r) = toISize $ sizeOf m

colsToMatrix :: Int -> [ImageInt] -> Matrix
colsToMatrix sz is = foldl addColumn2 (mtMatrix (0,sz)) is
 where mtMatrix sz = unsafePerformIO (emptyMatrix sz)

