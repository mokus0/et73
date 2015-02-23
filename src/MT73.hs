{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{- 
    mt73
    
    first iteration of a tool to receive and decode the signal from an
    older wireless dual-probe grill thermometer (Maverick ET-73)
 -}
module Main where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Data.Complex
import Data.Enumerator ((=$=), (=$), Iteratee(..), Enumeratee, Step(..), Stream(..))
import qualified Data.Enumerator.List as EL
import Data.IORef
import qualified Data.Vector.Storable as ST
import qualified Data.Vector.Unboxed as U
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import qualified RTLSDR
--import Text.Printf

type C = Complex Float

data IQ = IQ !Word8 !Word8

instance Storable IQ where
    alignment   _ = 2
    sizeOf      _ = 2
    peek p = do
        i <- peekByteOff p 0
        q <- peekByteOff p 1
        return (IQ i q)
    poke p (IQ i q) = do
        pokeByteOff p 0 i
        pokeByteOff p 1 q

toC :: IQ -> C
toC (IQ i q) = (fromIntegral i - 127) :+ (fromIntegral q - 127)

center      = 433.846e6
sampling    = 250e3
width       = 4e3
transition  = 1e3
decimation  = 10

fir :: U.Vector C
fir = U.fromList
    [  1.25053104e-01, -8.02281589e-02, -6.00878685e-02, -4.60099803e-02
    , -3.61592515e-02, -2.92184776e-02, -2.42541458e-02, -2.06114469e-02
    , -1.78254223e-02, -1.55795556e-02, -1.36566340e-02, -1.19078097e-02
    , -1.02557173e-02, -8.65549467e-03, -7.07265391e-03, -5.51575049e-03
    , -4.01603916e-03, -2.59743136e-03, -1.30115122e-03, -1.41122980e-04
    ,  7.72293092e-04,  1.63790262e-03,  1.87320993e-03,  2.29676345e-03
    ,  2.10961160e-03,  1.55020849e-03,  8.82047487e-04, -9.05462065e-05
    , -1.47999681e-03, -3.14528314e-03, -4.93677844e-03, -6.82765854e-03
    , -8.84539444e-03, -1.09645342e-02, -1.30655872e-02, -1.50176395e-02
    , -1.67535562e-02, -1.82205586e-02, -1.93625576e-02, -2.01512901e-02
    , -2.05056450e-02, -2.03672517e-02, -1.96427753e-02, -1.83988678e-02
    , -1.64960404e-02, -1.41321319e-02, -1.12182346e-02, -7.75549717e-03
    , -3.88090860e-03,  3.91716686e-04,  5.03877793e-03,  9.93999483e-03
    ,  1.49845622e-02,  2.00991623e-02,  2.51956351e-02,  3.01586986e-02
    ,  3.48556395e-02,  3.91863623e-02,  4.31131865e-02,  4.65625914e-02
    ,  4.94030919e-02,  5.15958396e-02,  5.30954044e-02,  5.38400859e-02
    ,  5.38400859e-02,  5.30954044e-02,  5.15958396e-02,  4.94030919e-02
    ,  4.65625914e-02,  4.31131865e-02,  3.91863623e-02,  3.48556395e-02
    ,  3.01586986e-02,  2.51956351e-02,  2.00991623e-02,  1.49845622e-02
    ,  9.93999483e-03,  5.03877793e-03,  3.91716686e-04, -3.88090860e-03
    , -7.75549717e-03, -1.12182346e-02, -1.41321319e-02, -1.64960404e-02
    , -1.83988678e-02, -1.96427753e-02, -2.03672517e-02, -2.05056450e-02
    , -2.01512901e-02, -1.93625576e-02, -1.82205586e-02, -1.67535562e-02
    , -1.50176395e-02, -1.30655872e-02, -1.09645342e-02, -8.84539444e-03
    , -6.82765854e-03, -4.93677844e-03, -3.14528314e-03, -1.47999681e-03
    , -9.05462065e-05,  8.82047487e-04,  1.55020849e-03,  2.10961160e-03
    ,  2.29676345e-03,  1.87320993e-03,  1.63790262e-03,  7.72293092e-04
    , -1.41122980e-04, -1.30115122e-03, -2.59743136e-03, -4.01603916e-03
    , -5.51575049e-03, -7.07265391e-03, -8.65549467e-03, -1.02557173e-02
    , -1.19078097e-02, -1.36566340e-02, -1.55795556e-02, -1.78254223e-02
    , -2.06114469e-02, -2.42541458e-02, -2.92184776e-02, -3.61592515e-02
    , -4.60099803e-02, -6.00878685e-02, -8.02281589e-02,  1.25053104e-01]

fir2 :: U.Vector C
fir2 = U.fromList
    [ -3.94449375e-04,  1.73750917e-02,  2.88815026e-03, -1.30252066e-02
    , -8.83062510e-03,  1.53808516e-02,  1.83464766e-02, -1.50943892e-02
    , -3.20907827e-02,  9.85336146e-03,  5.18838811e-02,  5.16582332e-03
    , -8.47537948e-02, -4.76800429e-02,  1.79537722e-01,  4.13096209e-01
    ,  4.13096209e-01,  1.79537722e-01, -4.76800429e-02, -8.47537948e-02
    ,  5.16582332e-03,  5.18838811e-02,  9.85336146e-03, -3.20907827e-02
    , -1.50943892e-02,  1.83464766e-02,  1.53808516e-02, -8.83062510e-03
    , -1.30252066e-02,  2.88815026e-03,  1.73750917e-02, -3.94449375e-04]

packBuf :: Ptr CUChar -> Int -> IO (ST.Vector IQ)
packBuf p_buf len = do
    let elems = len `div` sizeOf (IQ 0 0)
    
    fp_copy <- liftIO (mallocForeignPtrArray elems)
    liftIO $ withForeignPtr fp_copy $ \p_copy ->
        copyArray p_copy (castPtr p_buf) elems
    return $! ST.unsafeFromForeignPtr fp_copy 0 elems

readIter :: RTLSDR.RTLSDR -> Word32 -> Word32 -> Iteratee (ST.Vector IQ) IO a -> IO (Maybe a)
readIter rtl bufNum bufLen iter = do
    next <- runIteratee iter
    
    case next of
        Error e       -> E.throw e
        Yield a _     -> do
            return (Just a)
        Continue{}    -> do
            r_next <- newIORef next
            
            ok <- RTLSDR.readAsync rtl bufNum bufLen $ \p_buf len -> do
                step <- readIORef r_next
                case step of
                    Continue k    -> do
                        chunk <- packBuf p_buf len
                        runIteratee (k (Chunks [chunk])) >>= writeIORef r_next
                        
                    _               -> do
                        RTLSDR.cancelAsync rtl
                        writeIORef r_next step
            
            if ok
                then do
                    end <- readIORef r_next
                    case end of
                        Error e       -> E.throw e
                        Yield a _     -> return (Just a)
                        Continue{}    -> return Nothing
                else return Nothing

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

lfilter :: (Fractional t, U.Unbox t, Monad m) => U.Vector t -> U.Vector t -> Enumeratee t t m b
lfilter b' a' = flip EL.mapAccum (U.replicate (n-1) 0) $ \z x ->
    let z_0 = U.head z
        z_t = U.tail z
        
        y = (b_0 * x + z_0) / a_0
        
        z' = U.zipWith3 (\a b z -> b * x + z - a * y) a_t b_t (z_t `U.snoc` 0)
     in (z', y)
    where
        n = max (U.length a') (U.length b')
        
        pad v = U.generate n $ \i ->
            if i < U.length v 
                then v U.! i
                else 0
        
        a = pad a'
        b = pad b'
        
        a_0 = U.head a
        a_t = U.tail a
        
        b_0 = U.head b
        b_t = U.tail b

decimate :: Monad m => Int -> Enumeratee a a m b
decimate n = flip EL.concatMapAccum n $ \i x ->
    if i == n
        then (1,     [x])
        else (i + 1, [ ])

readFiltered :: RTLSDR.RTLSDR -> Word32 -> Word32 -> Iteratee C IO a -> IO (Maybe a)
readFiltered rtl bufNum bufLen iter = do
    readIter rtl bufNum bufLen
        (     EL.concatMap (map toC . ST.toList)
        =$= lfilter fir (U.singleton 1) =$= decimate 16
        -- =$= firdec
        -- =$= firdec
        -- =$= firdec
        -- =$= firdec
        =$  iter
        )
    where
        --firdec = lfilter fir2 (U.singleton 1) =$= decimate 2

magSq :: Num a => Complex a -> a
magSq (a :+ b) = a*a + b*b

db :: Floating a => a -> a
db z = (10 / log 10) * log z

-- all of these could be implemented without the extra IO layer, but in order
-- to share state between invocations they need to be this way

avgPwr :: (Floating a, U.Unbox a, Monad m) => a -> Enumeratee (Complex a) a m b
avgPwr alpha = 
    EL.map magSq
    
    -- simple exponential decay filter:
    -- y[m] = alpha * x[m] + (1 - alpha)*y[m-1]
    =$= lfilter
        (U.fromList [1, alpha])
        (U.fromList [1, alpha - 1])
    
    =$= EL.map db

indexed :: Monad m => Enumeratee a (Integer, a) m b
indexed = flip EL.mapAccum 0 $ \i x -> (i+1,(i,x))

trigger :: Monad m => (a -> Bool) -> Enumeratee a a m b
trigger p = flip EL.concatMapAccum False f
    where
        f fired x = (fire, if fire && not fired then [x] else [])
            where fire = p x

trigger_ :: Monad m => (a -> Bool) -> Enumeratee a Integer m b
trigger_ p = indexed =$= trigger (p . snd) =$= EL.map fst

diff :: (Num a, Monad m) => Enumeratee a a m b
diff = flip EL.mapAccum 0 $ \prev x -> (x, x - prev)

triggerIntervals :: Monad m => (a -> Bool) -> Enumeratee a Integer m b
triggerIntervals p = trigger_ p =$= diff

dump :: Show a => Iteratee a IO ()
dump = EL.mapM_ print

interp :: Iteratee Integer IO ()
interp = EL.mapM_ $ \dt ->
    when (dt > 13) $ putChar $
        if dt > 40 then '\n'
            else if dt > 22 then '1'
                else '0'

-- supported gains:
-- [0,9,14,27,37,77,87,125,144,157,166,197,207,229,254,280,297,328,338,364,372,386,402,421,434,439,445,480,496]
gain        = 229 -- tenths of db
threshold   = 0

test = do
    mbDev <- RTLSDR.open 0
    case mbDev of
        Nothing  -> return Nothing
        Just dev -> do
            RTLSDR.setSampleRate dev (round sampling)
            RTLSDR.setCenterFreq dev (round center)
            RTLSDR.setAGCMode dev False
            RTLSDR.setTunerGainMode dev True
            RTLSDR.setTunerGain dev gain
            RTLSDR.resetBuffer dev
            
            readFiltered dev 0 0
                (triggerIntervals ((> threshold) . db . magSq) =$ interp)
                -- (indexed =$ dump)
                -- (avgPwr 0.01 =$= decimate 100 =$ dump)

main = test
