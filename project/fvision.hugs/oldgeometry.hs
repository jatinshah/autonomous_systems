
-- This adds primitives to Fran.  These should move into Fran someday.

-- NOT PART OF FVISION (at present)


module Geometry( module Geometry 
               , module BaseTypes
               , module VectorSpace
               , module Vector2
               , module Point2
               , module Rect
--       , module Transform2
               , module Transform
               , module Vector3
               , module Point3
               , module Transform3
   ) where

-- Build a basic geometry module out of the Fran geometry with some
-- additions.  For now, don't change Fran.
-- Make sure Fran is on your search path!  Download it from
-- Conal: http://www.research.microsoft.com/~conal/newfran


import StaticTypesCore hiding (
           Transform2
         , Transformable2
         , (*%)
         , translateUscaleRotate2
         , identity2             
         , translate2            
         , rotate2               
         , compose2              
         , uscale2               
         , inverse2              
         , factorTransform2
         , Color
         )
-- import qualified XVision as XV
import XVTypes
import Transform
 
{- imported from Fran:
Naming conventions:
  .  = point
  ^  = vectorr
Thus, ^+^ is vector addition, .+^ is addition of a point and a vector.

The distinction between point and vector is sometimes annoying!  Watch
for type error.  Note the points are NOT in Num, only vectors.

Note that everything uses Double rather than Float.  Probably change XVision
to use Double exclusively.

More overloading will be possible once multi-parameter type classes
become available.

Type synonyms:

type RealVal  = Double
type Length   = RealVal
type Radians  = RealVal
type Fraction = RealVal  -- 0 to 1 (inclusive)
type Scalar   = Double

class Transformable2 a where
  (*%)  ::  Transform2 -> a -> a


General Vector Spaces:

infixr 7 *^, ^/, `dot`
infixl 6 ^+^, ^-^

class VectorSpace v where
  zeroVector      :: v
  (*^)            :: Scalar -> v -> v
  (^+^)           :: v -> v -> v
  dot             :: v -> v -> Scalar

(^-^)             :: VectorSpace v => v -> v -> v
(^/)              :: VectorSpace v => v -> Scalar -> v
negateVector      :: VectorSpace v =>  v -> v
magnitudeSquared  :: VectorSpace v =>  v -> Scalar
magnitude         :: VectorSpace v =>  v -> Scalar
normalize         :: VectorSpace v =>  v -> v
lerpVS            :: VectorSpace v => v -> v -> Scalar -> v  -- interpolate

instance  VectorSpace Double  where
instance (VectorSpace a, VectorSpace b) => VectorSpace (a,b) where


2D points

infix 4 .+^, .-^, .-.

data Point2 = Point2XY RealVal RealVal

origin2            :: Point2
point2XY           :: RealVal    -> RealVal    -> Point2
point2Polar        :: Radians -> Length    -> Point2
point2XYCoords     :: Point2  -> (RealVal, RealVal)
point2PolarCoords  :: Point2  -> (Length, Radians)
distance2          :: Point2 -> Point2 -> Length
distance2Squared   :: Point2 -> Point2 -> Length
linearInterpolate2 :: Point2 -> Point2 -> RealVal -> Point2
p2v                :: Point2 -> Vector2
v2p                :: Vector2 -> Point2
(.+^)              :: Point2 -> Vector2 -> Point2
(.-^)              :: Point2 -> Vector2 -> Point2
(.-.)              :: Point2 -> Point2  -> Vector2

instance Transformable2 Point2

2D Vectors:

data Vector2 = Vector2XY RealVal RealVal

xVector2, yVector2 :: Vector2, Vector2  -- unit vectors
vector2XY          :: RealVal -> RealVal -> Vector2
vector2Polar       :: Length  -> Radians -> Vector2
vector2XYCoords    :: Vector2 -> (RealVal, RealVal)
vector2PolarCoords :: Vector2 -> (Length, Radians)
rotateVector2      :: RealVal -> Vector2 -> Vector2

instance  Num Vector2
instance  VectorSpace Vector2
instance  Transformable2 Vector2  -- ignores displacement

2D Transformation

-- This probably needs work since scaling may not be uniform.

class Transformable2 a where
  (*%)             ::  Transform2 -> a -> a

translateUscaleRotate2 ::  Vector2 -> RealVal -> Radians -> Transform2
identity2           :: Transform2
translate2          :: Vector2 -> Transform2
rotate2             :: Radians -> Transform2
compose2            :: Transform2 -> Transform2 -> Transform2
uscale2             :: RealVal -> Transform2
inverse2            :: Transform2 -> Transform2
factorTransform2    :: Transform2 -> (Vector2, RealVal, Radians)

instance (Transformable2 a, Transformable2 b) => Transformable2 (a,b)
instance (Transformable2 a, Transformable2 b, Transformable2 c)
  => Transformable2 (a,b,c)

3D Points:

infix 4 .+^#, .-^#, .-.#

data Point3 = Point3XYZ RealVal RealVal RealVal

origin3             :: Point3
point3XYZ           :: RealVal -> RealVal -> Point3
point3XYZCoords     :: Point3  -> (RealVal, RealVal)
distance3           :: Point3 -> Point3 -> Length
distance3Squared    :: Point3 -> Point3 -> Length
linearInterpolate3  :: Point3 -> Point3 -> RealVal -> Point3
(.+^#)              :: Point3 -> Vector3 -> Point3
(.-^#)              :: Point3 -> Vector3 -> Point3
(.-.#)              :: Point3 -> Point3  -> Vector3


3D Vectors:

data Vector3 = Vector3XYZ RealVal RealVal RealVal

xVector3, yVector3, zVector3 :: Vector3  -- unit vectors
vector3XYZ             :: RealVal -> RealVal -> RealVal -> Vector3
vector3XYZCoords       :: Vector3 -> (RealVal, RealVal)
vector3Spherical       :: Length  -> Radians -> Vector3
vector3SphericalCoords :: Vector3 -> (Length, Radians)

instance  VectorSpace Vector3
instance  Num Vector3


-- Note: no Transform3 yet! 

-- Note: Conal has a bunch of 2D rectangle code we might want

-}

----------------------------------------------------------------
-- Integer point conversions
----------------------------------------------------------------

iPointToPoint2 :: IPoint -> Point2
iPointToPoint2 (x, y) = point2XY (fromInt x) (fromInt y)


point2ToIPoint :: Point2 -> IPoint
point2ToIPoint (Point2XY x y) =  (round x, round y)


---------------------------------------------------------------
-- 2D Rectangular sizes
---------------------------------------------------------------

iSize :: Int -> Int -> Size
iSize x y = Size (fromInt x) (fromInt y)

toSize :: ISize -> Size
toSize (x,y) = iSize x y
sizeWidth (Size x y) = x
sizeHeight (Size x y) = y

toISize (Size x y) = (round x, round y) 

showSized :: Sized a => String -> a -> ShowS
showSized nm x = showString "<<" . showString nm .
                 showString " " .
                 shows w . showString " x " .
                 shows h . showString " >>"
  where
    (w,h) = toISize $ sizeOf x  
 
transposeSize (Size x y) = Size y x

instance Num Size where
  Size x y + Size a b = Size (x+a) (y+b)
  Size x y - Size a b = Size (x-a) (y-b)  --?? check for positive sizes?
  fromInteger i = Size (fromInteger i) (fromInteger i)
  negate (Size x y) = Size (-x) (-y)   ---????

-- Should be more general

areaOf :: Size -> RealVal
areaOf (Size x y) = x*y

----------------
-- Vectors
----------------

rotate :: Transformable2 a => Radians -> a -> a
rotate a' v  = rotate2 a' *% v

lengthVector2 :: Vector2 -> Unit
lengthVector2 v = l where (l,a) = vector2PolarCoords v

-- scale a vector to be a certain length
-- the use of signum is pure paranoia
scaledTo :: Vector2 -> Unit -> Vector2
scaledTo v l = vector2Polar (l*signum r) a
 where
  (r,a) = vector2PolarCoords v

----------------
-- Line Segments
----------------




-- invert a 2x2 matrix 
-- Oops! Blow up when the determinate is 0!

inverse :: (RealVal,RealVal,RealVal,RealVal) -> (RealVal,RealVal,RealVal,Double)
inverse (a,b,c,d) = (d/det, -c/det, -b/det, a/det)
 where
  det = a*d - b*c

-- This is the C++ code - but I can't understand it
--
--  line2D l1 = line1->coordinates();
--  line2D l2 = line2->coordinates();
--
--  float den = l1.x * l2.y - l2.x * l1.y;
--
--  if (fabs(den) < TINY) return 0;
--
--  x = -(l2.y * l1.z - l1.y * l2.z)/den;
--  y = (l2.x * l1.z - l1.x * l2.z)/den;

----------------
-- Intersection
----------------

mkIntersection :: Point2 -> Vector2 -> Vector2 -> Intersection
mkIntersection = (,,)

intersectionPoint :: Intersection -> Point2
intersectionPoint (p,_,_) = p

----------------------------------------------------------------
-- End
----------------------------------------------------------------

{-

Names to change:

Angle to Radians
Pt to Point2
Vector to Vector2
asCartesian to vector2XYCoords
asPolar to Vector2PolarCoords
vscale to *^

Not implemented:

-- offsets to apply to Lines (of various kinds)
-- (raw data returned by Edge trackers)


-- infinite line: stretches to infinity in each direction
type InfLine = (Pt, Angle) -- (centre, direction)

-- really just another way of specifying a region
type Edge = (InfLine,Size)

square :: Num a => a -> a
square x = x * x

-- intersection between two lines
-- A similar but different type would use two angles instead of vectors
type Intersection = (Pt, Vector, Vector)

spos :: Intersection -> Pt
pos (pt,_,_) = pt

mkIntersection :: Pt -> Vector -> Vector -> Intersection
mkIntersection = (,,)

mkIntersection2 :: Pt -> Pt -> Pt -> Intersection
mkIntersection2 p1 p2 p3 = (p2, p1 `subPt` p2, p3 `subPt` p2)

----------------
-- Misc
----------------

-}
