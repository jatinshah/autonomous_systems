
-- This module will be replaced by Fran DBehaviors when integrated with Frob. 

----------------------------------------------------------------
-- Pipes
----------------------------------------------------------------

module Pipe
	( module Pipe
	) where

import IOExts
import XVision
import Prelude hiding(sum)
import Monad
import Maybe
import XVUtilities
import List(zipWith4)

----------------------------------------------------------------
-- Standard Types
----------------------------------------------------------------

--type Time = Int

newtype Pipe a = Pipe [a]

unP :: Pipe a -> [a]
unP (Pipe xs) = xs

----------------------------------------------------------------
-- Events - same as in Fran
----------------------------------------------------------------

type Event a = Pipe (Maybe a)

untilP    :: Pipe a  -> Event (Pipe a) -> Pipe a
snapshot  :: Event a -> Pipe b -> Event (a, b)

neverE    :: Event a
(.|.)     :: Event a -> Event a -> Event a
anyE      :: [Event a] -> Event a
(==>)     :: Event a -> (a -> b) -> Event b

predicate :: Pipe Bool -> Event ()

-- Not done yet
-- whenE     :: Event a -> B Bool -> Event a
-- joinEOne  :: Event (Event a) -> Event a
-- suchThat  :: Event a -> (a -> Bool) -> Event a

----------------------------------------------------------------

neverE  = lift0 Nothing
(.|.)   = lift2 (mplus)
anyE    = foldr (.|.) neverE
m ==> f = lift1 (fmap f) m

predicate bs = lift1 (\ b -> if b then Just () else Nothing) bs

snapshot es bs = lift2 snapshot' es bs
 where
  (Just e) `snapshot'` b = Just (e,b)
  Nothing  `snapshot'` b = Nothing

a `untilP` b = Pipe (unP a `until'` unP b)
 where
  -- order matters if you want things to be lazy enough
  _      `until'` (Just ys : _)  = unP ys
  (x:xs) `until'` (Nothing : ys) = x : (xs `until'` ys)

----------------------------------------------------------------
-- Standard combinators
--
-- Note that lift<n> were called pipe<n> in the paper
----------------------------------------------------------------

runPipe :: Pipe (IO a) -> IO ()
runPipe as = sequence_ (unP as)

runPipeGC :: Pipe (IO a) -> IO ()
runPipeGC as = runPipe (lift1 (\a -> do { a; garbageCollect }) as)

-- Puts forwarding into the pipe; this should be covered up 
-- in the video sources themselves, somehow....

runPipeFor :: IO() -> Pipe (IO a) -> IO ()
runPipeFor forfun as = runPipe (lift1 (\a -> do { a; forfun; garbageCollect }) as)

-- sample a subpipe n times faster than its input
oversample :: Int -> Pipe a -> Pipe a
oversample n as = Pipe $ concatMap (replicate n) (unP as)

-- sample a pipe n times slower than it generates data
-- could be written more efficiently
subsample :: Int -> Pipe a -> Pipe a
subsample n as = Pipe $ map last $ chop n $ unP as
 where
  chop n as = bs : chop n cs
   where
    (bs,cs) = splitAt n as

listToPipe :: [a] -> Pipe a
listToPipe = Pipe

lift0 :: a -> Pipe a
lift0 a = Pipe $ repeat a

lift1 :: (a -> b) -> (Pipe a -> Pipe b)
lift1 f as = Pipe $ map f (unP as)

lift2 :: (a -> b -> c) -> (Pipe a -> Pipe b -> Pipe c)
lift2 f as bs = Pipe $ zipWith f (unP as) (unP bs)

lift3 :: (a -> b -> c -> d) -> (Pipe a -> Pipe b -> Pipe c -> Pipe d)
lift3 f as bs cs = Pipe $ zipWith3 f (unP as) (unP bs) (unP cs)

lift4 :: (a -> b -> c -> d -> e) -> (Pipe a -> Pipe b -> Pipe c -> Pipe d -> Pipe e)
lift4 f as bs cs ds = Pipe $ zipWith4 f (unP as) (unP bs) (unP cs) (unP ds)

liftIO0 :: IO a -> Pipe a
liftIO0 f = Pipe $ unsafePerformIO (mkLazyList f)

liftIO1 :: (a -> IO b) -> (Pipe a -> Pipe b)
liftIO1 f as = Pipe $ unsafePerformIO (mkLazyList' (map f (unP as)))

liftIO2 :: (a -> b -> IO c) -> (Pipe a -> Pipe b -> Pipe c)
liftIO2 f as bs = Pipe $ unsafePerformIO (mkLazyList' (zipWith f (unP as) (unP bs)))

-- delay by one step using default value at first step
delay :: a -> Pipe a -> Pipe a
delay a as = Pipe $ a : (unP as)

-- delay by one step by duplicating first value
delay' :: Pipe a -> Pipe a
delay' (Pipe as) = Pipe $ head as : as

-- advance by one step
advance	:: Pipe a -> Pipe a
advance	(Pipe [])     = Pipe []
advance	(Pipe (a:as)) = Pipe as

scanPipe :: (a -> a -> a) -> a -> Pipe a -> Pipe a
scanPipe f a as = Pipe $ scanl f a (unP as)

scanPipe1 :: (a -> a -> a) -> Pipe a -> Pipe a
scanPipe1 f as = Pipe $ scanl1 f (unP as)

----------------------------------------------------------------
-- Pipe utilitiesrIO
--
-- Note that these were called split<n> in the paper.
----------------------------------------------------------------

-- somewhat redundant!
drop1 :: Pipe (a) -> (Pipe a)
drop1 x = x

drop2 :: Pipe (a,b) -> (Pipe a, Pipe b)
drop2 xy = (lift1 fst xy, lift1 snd xy)

drop3 :: Pipe (a,b,c) -> (Pipe a, Pipe b, Pipe c)
drop3 xyz = (lift1 fst3 xyz, lift1 snd3 xyz, lift1 thd3 xyz)
 where
  fst3 (x,y,z) = x
  snd3 (x,y,z) = y
  thd3 (x,y,z) = z

drop4 :: Pipe (a,b,c,d) -> (Pipe a, Pipe b, Pipe c, Pipe d)
drop4 as = (lift1 pi1 as, lift1 pi2 as, lift1 pi3 as, lift1 pi4 as)
 where
  pi1 (a1,a2,a3,a4) = a1
  pi2 (a1,a2,a3,a4) = a2
  pi3 (a1,a2,a3,a4) = a3
  pi4 (a1,a2,a3,a4) = a4


join2 :: (Pipe a, Pipe b) -> Pipe (a,b)
join2 (x,y) = lift2 (,) x y


----------------------------------------------------------------
-- Generalisations of the tuple ops to lists
--
-- Note that these use the lazier version of transpose
-- being proposed for Haskell'98
----------------------------------------------------------------

listp_to_plist :: [Pipe a] -> Pipe [a]
plist_to_listp :: Pipe [a] -> [Pipe a]

listp_to_plist ps = Pipe (transpose98 (map unP ps))
plist_to_listp p  = map Pipe (transpose98 (unP p))

--Proposed Haskell'98 code:
transpose98             :: [[a]] -> [[a]]
transpose98             =  foldr
                             (\xs xss -> zipLazier (:) xs (xss ++ repeat []))
                             []
 where
  zipLazier f (x:xs) xss = f x (head xss) : zipLazier f xs (tail xss)
  zipLazier _ _      _   = []


----------------------------------------------------------------
-- Pipe utilities
----------------------------------------------------------------

integral :: (Num a) => a -> Pipe a -> Pipe a
integral = scanPipe (+)

-- Warning: this leaks space (for all the usual reasons) - don't use
-- time :: Pipe Time
-- time = listToPipe [0..]

----------------------------------------------------------------
-- Type class abuse
----------------------------------------------------------------

instance (Eq a)   => Eq   (Pipe a)   -- bogus
instance (Show a) => Show (Pipe a) where
  showsPrec d (Pipe as) r = showsPrec d as r
instance (Real a) => Real (Pipe a)   -- bogus
instance (Enum a) => Enum (Pipe a)   -- bogus

instance (Num a) => Num (Pipe a) where
  (+)  	      = lift2 (+)
  (-)  	      = lift2 (-)
  (*)  	      = lift2 (*)
  negate      = lift1 negate
  abs         = lift1 abs
  fromInteger = lift0 . fromInteger
  fromInt     = lift0 . fromInt

instance (Ord a) => Ord (Pipe a)  where
  min = lift2 min
  max = lift2 max

instance (Integral a) => Integral (Pipe a) where
  quot = lift2 quot
  rem  = lift2 rem
  div  = lift2 div
  mod  = lift2 mod

instance (Fractional a) => Fractional (Pipe a) where
  fromDouble   = lift0 . fromDouble
  fromRational = lift0 . fromRational
  (/)          = lift2 (/)

instance (Floating a) => Floating (Pipe a) where
  sin     = lift1 sin
  cos     = lift1 cos
  tan     = lift1 tan
  asin    = lift1 asin
  acos    = lift1 acos
  atan    = lift1 atan
  sinh    = lift1 sinh
  cosh    = lift1 cosh
  tanh    = lift1 tanh
  asinh   = lift1 asinh
  acosh   = lift1 acosh
  atanh   = lift1 atanh
  pi      = lift0 pi
  exp     = lift1 exp
  log     = lift1 log
  sqrt    = lift1 sqrt
  (**)    = lift2 (**)
  logBase = lift2 logBase

----------------------------------------------------------------
-- End
----------------------------------------------------------------
