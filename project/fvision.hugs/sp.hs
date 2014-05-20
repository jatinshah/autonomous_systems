
-- OLD CODE.  NOT YET WORKING!

-- Convert to DBehaviors

----------------------------------------------------------------
-- Stream Processors
--
-- Choice of symbols is inspired by John Hughes' Arrow combinators
----------------------------------------------------------------

--- TODO: Separate out the code relevant ot tracking from generic 
--- stream processor code


module SP
        ( module SP
        ) where

-- import CStream
import IOExts( unsafePerformIO )
import XVTypes 
import Error
import FRP

-- Ought to be in XVTypes:
type SP clk a b = VCEvent clk a -> VCEvent clk b

-- Ought to be in XVTypes:
-- Not clear if this should be here - but it is used an awful lot
type Tracker clk a = SP clk a (WithError a)


-- loopSP :: a -> (b -> a) -> SP clk a b -> VCEvent clk b
-- loopSP a0 f sp = let b = sp (delay a0 a)
--                      a = lift1S f b
--                  in b

sp0 :: b -> SP clk a b
sp0 b as = lift0 b

sp1 :: (a -> b) -> SP clk a b
sp1 f as = lift1 f as

sp2 :: (a -> b -> c) -> SP clk (a,b) c
sp2 f as = lift2 f (lift1 fst as) (lift1 snd as)

sp3 :: (a -> b -> c -> d) -> SP clk (a,b,c) d
sp3 f as = lift3 f (lift1 pi1 as) (lift1 pi2 as) (lift1 pi3 as)
 where
  pi1 (a,b,c) = a
  pi2 (a,b,c) = b
  pi3 (a,b,c) = c

sp4 :: (a -> b -> c -> d -> e) -> SP clk (a,b,c,d) e
sp4 f as = lift4 f (lift1 pi1 as) (lift1 pi2 as) (lift1 pi3 as) (lift1 pi4 as)
 where
  pi1 (a,b,c,d) = a
  pi2 (a,b,c,d) = b
  pi3 (a,b,c,d) = c
  pi4 (a,b,c,d) = d

--spIO1 :: (a -> IO b) -> SP clk a b
--spIO1 f as = liftIO1S f as

--spTrace :: (a -> IO ()) -> SP clk a a
--spTrace f as = liftIO1S (\a -> do { f a; return a }) as

-- A stateful stream processor 
--statefulSP :: IO s -> (s -> a -> IO b) -> SP clk a b
--statefulSP init step = unsafePerformIO $ do
--  { s <- init
--  ; return (spIO1 (step s))
--  }

(>>>) :: SP clk a b -> SP clk b c -> SP clk a c
(f >>> g) as = g (f as)

(&&&) :: SP clk a b -> SP clk a c -> SP clk a (b,c)
(f &&& g) as = lift2 (,) (f as) (g as)

(***) :: SP clk a1 b1 -> SP clk a2 b2 -> SP clk (a1,a2) (b1,b2)
(f *** g) as = lift2 (,) (f (lift1 fst as)) (g (lift1 snd as))

-- same as ***

par2 :: SP clk a1 b1 -> SP clk a2 b2 -> SP clk (a1,a2) (b1,b2)
par2 f1 f2 as = lift2 (,) (f1 (lift1 fst as)) (f2 (lift1 snd as))

par3 :: SP clk a1 b1 
     -> SP clk a2 b2 
     -> SP clk a3 b3
     -> SP clk (a1,a2,a3) (b1,b2,b3)
par3 f1 f2 f3 as = 
  lift3 (,,) 
    ((f1.lift1 pi1) as)
    ((f2.lift1 pi2) as)
    ((f3.lift1 pi3) as)
 where
  pi1 (a1,a2,a3) = a1
  pi2 (a1,a2,a3) = a2
  pi3 (a1,a2,a3) = a3

par4 :: SP clk a1 b1 
     -> SP clk a2 b2 
     -> SP clk a3 b3
     -> SP clk a4 b4 
     -> SP clk (a1,a2,a3,a4) (b1,b2,b3,b4)
par4 f1 f2 f3 f4 as = 
  lift4 (,,,) 
    ((f1.lift1 pi1) as)
    ((f2.lift1 pi2) as)
    ((f3.lift1 pi3) as)
    ((f4.lift1 pi4) as)
 where
  pi1 (a1,a2,a3,a4) = a1
  pi2 (a1,a2,a3,a4) = a2
  pi3 (a1,a2,a3,a4) = a3
  pi4 (a1,a2,a3,a4) = a4


{-
----------------------------------------------------------------
-- Building composite trackers out of simple trackers
----------------------------------------------------------------

composite2 :: (a -> (a1,a2)) 
           -> (a1 -> a2 -> a)
           -> Tracker clk a1
           -> Tracker clk a2
           -> Tracker clk a
composite2 split join t1 t2 = sp1 split >>> par2 t1 t2 >>> sp2 (liftE2 join)

composite3 :: (a -> (a1,a2,a3)) 
           -> (a1 -> a2 -> a3 -> a)
           -> Tracker clk a1
           -> Tracker clk a2
           -> Tracker clk a3
           -> Tracker clk a
composite3 split join t1 t2 t3 = sp1 split >>> par3 t1 t2 t3 >>> sp3 (liftE3 join)

composite4 :: (a -> (a1,a2,a3,a4)) 
           -> (a1 -> a2 -> a3 -> a4 -> a)
           -> Tracker clk a1
           -> Tracker clk a2
           -> Tracker clk a3
           -> Tracker clk a4
           -> Tracker clk a
composite4 split join t1 t2 t3 t4 = sp1 split >>> par4 t1 t2 t3 t4 >>> sp4 (liftE4 join)



----------------------------------------------------------------
-- Tracing trackers
----------------------------------------------------------------

traceSP :: (Show a) => String -> Tracker clk a -> Tracker clk a
traceSP s t = spTrace before >>> t >>> spTrace after
 where
  before x = putStrLn $ s ++ " before: " ++ show x
  after  x = putStrLn $ s ++ " after:  " ++ show x

----------------------------------------------------------------
-- End
----------------------------------------------------------------

-}
