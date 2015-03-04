{-# LANGUAGE BangPatterns #-}
module Data.Enumerator.Signal where

import Data.Enumerator (Enumeratee)
import qualified Data.Enumerator.List as EL
import qualified Data.Vector.Unboxed as U

-- given filter tap vectors A and B, this implements a FIR filter
-- which maps input stream X to output stream Y according to the
-- definition:
--
--      a[0]*y[n] = b[0]*x[n] + b[1]*x[n-1] + ... + b[nb]*x[n-nb]
--                            - a[1]*y[n-1] - ... - a[na]*y[n-na]
--
-- using the transposed direct form 2:
--
--      y[m] = b[0]*x[m] + z[0,m-1]
--      z[0,m] = b[1]*x[m] + z[1,m-1] - a[1]*y[m]
--      ...
--      z[n-3,m] = b[n-2]*x[m] + z[n-2,m-1] - a[n-2]*y[m]
--      z[n-2,m] = b[n-1]*x[m] - a[n-1]*y[m]
--
-- (as described in scipy's lfilter documentation)
--
-- or, in pseudo-Haskell:
--
--      do
--          y <- (b ! 0) * x + z ! 0
--          yield (y / (a ! 0))
--          forM_ [0 .. n - 2] $ \i -> do
--              (z ! i) <- (b ! (i+1)) * x + (z ! (i+1)) - (a ! (i+1)) * y
--
--  (where out-of-bounds reads of 'a' and 'b' return 0)
{-# INLINE lfilter #-}
lfilter :: (Fractional t, U.Unbox t, Monad m) => U.Vector t -> U.Vector t -> Enumeratee t t m b
lfilter b' a' = flip EL.mapAccum (U.replicate (n-1) 0) $ \(!z) (!x) ->
    let z_0 = U.head z
        z_t = U.tail z `U.snoc` 0
        
        !y  = (b_0 * x + z_0) / a_0
        
        updateZ !a !b !z = b * x + z - a * y
     in (U.zipWith3 updateZ a_t b_t z_t, y)
    where
        n = max (U.length a') (U.length b')
        
        pad v = v U.++ U.replicate (n - U.length v) 0
        
        {-# NOINLINE a_0 #-}
        a_0 = U.head      a'
        
        {-# NOINLINE a_t #-}
        a_t = U.tail (pad a')
        
        {-# NOINLINE b_0 #-}
        b_0 = U.head      b'
        
        {-# NOINLINE b_t #-}
        b_t = U.tail (pad b')

-- discard out all but one of every 'n' samples
{-# INLINE decimate #-}
decimate :: Monad m => Int -> Enumeratee a a m b
decimate n = flip EL.concatMapAccum n $ \i x ->
    if i == n
        then (1,     [x])
        else (i + 1, [ ])

-- linear interpolation: "lerp x y" maps [0,1] to [x,y]
{-# INLINE lerp #-}
lerp x y alpha = alpha * y + (1 - alpha) * x

{-# INLINE dcReject #-}
dcReject :: (Fractional a, Monad m) => a -> Enumeratee a a m b
dcReject !alpha = EL.mapAccum f 0
    where
        f !dc !x = (lerp dc x alpha, x - dc)

