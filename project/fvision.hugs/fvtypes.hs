
-- Type definitions in the FVision core.  See the FVision paper in
-- PADL01 for more details.

module FVTypes where

import FRP
import XVTypes   -- types used in the underlying XV stuff
import NXVision2
import Animate

-- A signal containing video images.

type VideoB = Behavior VideoInput ImageRGB

-- Observer and Stepper types used in trackers.  An observer takes
-- the expected feature location and returns an observatiosn, usually
-- an image.

type Observer observation loc =
    (loc, ImageRGB) -> observation

-- A stepper updates the current location based on an observation.
-- The location is "measured" - that is, it contains extra 
-- information in addition to the location.

type Stepper measure observation loc =
    (loc, observation) -> measure loc

-- A tracker computes measured locations from the video feed.

type Tracker measure a = Stepper measure ImageRGB a

-- This extracts a measurement from a measured value.

class Valued c where
  valueOf :: c a -> a

-- Residual type (use by SSD).  Lower residuals are better.

data Residual a = 
  Residual { resValue :: a, residual :: FRPReal }
    deriving Show

instance Valued Residual where
  valueOf = resValue

instance Functor Residual where
  fmap f x = x {resValue = f (resValue x)}

instance Eq (Residual a) where
  x == y = residual x == residual y

instance Ord (Residual a) where
  r1 > r2 = residual r1 < residual r2

-- This is used by the edge tracker.  Higher sharpness is better.
-- The Ord instance is needed for bestOf / nudging.

data Sharpness a = 
  Sharpness { sValue :: a, sharpness :: FRPReal }
    deriving Show

instance Valued Sharpness where
  valueOf = sValue

instance Functor Sharpness where
  fmap f x = x {sValue = f (sValue x)}

instance Eq (Sharpness a) where
  x == y = sharpness x == sharpness y

instance Ord (Sharpness a) where
  r1 > r2 = sharpness r1 > sharpness r2

