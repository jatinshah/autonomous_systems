
-- This hasn't been integrated into FVision yet!!  

-- Compiles under new frp
-- OLD CODE    DOES NOT WORK!!!

----------------------------------------------------------------
-- Errors
--
-- ToDo: add sensible operations for manipulating errors estimates.
-- eg if Errors are in range [0..1] then we could have the usual
-- and, or, etc from probability

----------------------------------------------------------------
module Error
	( module Error
	) where

import XVTypes

infixl 9 `andError`
----------------------------------------------------------------
-- ToDo: should this be some kind of monad?
-- Use a lifted apply as the basis here.
-- Add a better Show method
----------------------------------------------------------------

errorOf :: WithError a -> Error
errorOf (WithError _ e) = e

valOf :: WithError a -> a
valOf (WithError v _) = v

andError :: Error -> Error -> Error
andError = max -- just one of many possible choices

eMin :: WithError a -> WithError a -> WithError a
eMin x y | errorOf x > errorOf x = y
         | otherwise = x

-- Also returns the index.

eMinL :: [WithError a] -> (WithError a, Int)
eMinL [] = error "[] in Error:eMinL"
eMinL [e] = (e,0)
eMinL (e:es) = let (e',r') = eMinL es in
   if errorOf e <= errorOf e' then (e,0) else (e',r'+1)

liftE1 :: (a -> b) -> WithError a -> WithError b
liftE1 f (WithError x e) = WithError (f x) e 

liftE2 :: (a -> b -> c) -> WithError a -> WithError b -> WithError c
liftE2 f (WithError x1 e1) (WithError x2 e2) =
     WithError (f x1 x2) (andError e1 e2)

liftE3 :: 
 (a -> b -> c -> d) -> WithError a -> WithError b -> WithError c -> WithError d
liftE3 f (WithError x1 e1) (WithError x2 e2) (WithError x3 e3) =
     WithError (f x1 x2 x3) (andError e1 (andError e2 e3))

liftE4 :: 
     (a -> b -> c -> d -> e) ->
     WithError a -> WithError b -> WithError c -> WithError d -> WithError e

liftE4 f (WithError x1 e1) (WithError x2 e2)
         (WithError x3 e3) (WithError x4 e4) =
     WithError (f x1 x2 x3 x4) (andError e1 (andError e2 (andError e3 e4)))

instance Functor WithError where
  fmap = liftE1

instance Num a => Num (WithError a) where
 (+) = liftE2 (+)
 (-) = liftE2 (-)
 (*) = liftE2 (*)
 negate = liftE1 negate

----------------------------------------------------------------
-- End
----------------------------------------------------------------