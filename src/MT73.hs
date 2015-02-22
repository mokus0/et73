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
import qualified Data.Vector.Unboxed.Mutable as MU
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
    [  1.05080393e-04,  5.54269869e-04,  9.53994929e-04,  1.17045281e-03
    ,  1.09640775e-03,  6.89879748e-04, -1.54156041e-18, -8.30191938e-04
    , -1.58883043e-03, -2.04531318e-03, -2.01454830e-03, -1.41860547e-03
    , -3.27235107e-04,  1.03784975e-03,  2.34169350e-03,  3.21311450e-03
    ,  3.34362859e-03,  2.58333248e-03,  1.00622137e-03, -1.07769200e-03
    , -3.17417801e-03, -4.71478813e-03, -5.20240438e-03, -4.35694566e-03
    , -2.22128769e-03,  8.06679668e-04,  4.03405158e-03,  6.62044051e-03
    ,  7.78434740e-03,  7.02040440e-03,  4.27085055e-03, -4.63153670e-18
    , -4.85998307e-03, -9.09361752e-03, -1.14847308e-02, -1.11357698e-02
    , -7.74583821e-03, -1.77108376e-03,  5.58783623e-03,  1.25893241e-02
    ,  1.73181176e-02,  1.81457514e-02,  1.41835508e-02,  5.61889702e-03
    , -6.15773605e-03, -1.86873620e-02, -2.88353827e-02, -3.33802514e-02
    , -2.96860912e-02, -1.63202422e-02,  6.52096285e-03,  3.68819733e-02
    ,  7.12595828e-02,  1.05132173e-01,  1.33714574e-01,  1.52796023e-01
    ,  1.59497287e-01,  1.52796023e-01,  1.33714574e-01,  1.05132173e-01
    ,  7.12595828e-02,  3.68819733e-02,  6.52096285e-03, -1.63202422e-02
    , -2.96860912e-02, -3.33802514e-02, -2.88353827e-02, -1.86873620e-02
    , -6.15773605e-03,  5.61889702e-03,  1.41835508e-02,  1.81457514e-02
    ,  1.73181176e-02,  1.25893241e-02,  5.58783623e-03, -1.77108376e-03
    , -7.74583821e-03, -1.11357698e-02, -1.14847308e-02, -9.09361752e-03
    , -4.85998307e-03, -4.63153670e-18,  4.27085055e-03,  7.02040440e-03
    ,  7.78434740e-03,  6.62044051e-03,  4.03405158e-03,  8.06679668e-04
    , -2.22128769e-03, -4.35694566e-03, -5.20240438e-03, -4.71478813e-03
    , -3.17417801e-03, -1.07769200e-03,  1.00622137e-03,  2.58333248e-03
    ,  3.34362859e-03,  3.21311450e-03,  2.34169350e-03,  1.03784975e-03
    , -3.27235107e-04, -1.41860547e-03, -2.01454830e-03, -2.04531318e-03
    , -1.58883043e-03, -8.30191938e-04, -1.54156041e-18,  6.89879748e-04
    ,  1.09640775e-03,  1.17045281e-03,  9.53994929e-04,  5.54269869e-04
    ,  1.05080393e-04]

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

lfilter :: (Fractional t, Eq t, U.Unbox t) => U.Vector t -> U.Vector t -> Enumeratee t t IO b
lfilter b a start = do
    let n = max (U.length a) (U.length b)
        
        pad v = U.generate n $ \i ->
            if i < U.length v 
                then v U.! i
                else 0
    
    a <- return $! pad a
    b <- return $! pad b
    
    let a_0 = U.head a
        a_t = U.tail a
        
        b_0 = U.head b
        b_t = U.tail b
    
    z <- liftIO (MU.replicate (n-1) 0)
    
    flip EL.mapM start $ \x -> do
        z_0 <- MU.read z 0
        let y = (b_0 * x + z_0) / a_0
            bx_m_ay = U.zipWith (-) (U.map (* x) b_t)
                                    (U.map (* y) a_t)
        
        sequence_
            [ do
                z_ip1 <- MU.read z (i + 1)
                MU.write z i (z_ip1 + (bx_m_ay U.! i))
            | i <- [0 .. n-3]
            ]
        
        MU.write z (n-2) (bx_m_ay U.! (n-2))
        
        return y

decimate :: Monad m => Int -> Enumeratee a a m b
decimate n = flip EL.concatMapAccum n $ \i x ->
    if i == n
        then (1,     [x])
        else (i + 1, [ ])

readFiltered :: RTLSDR.RTLSDR -> Word32 -> Word32 -> Iteratee C IO a -> IO (Maybe a)
readFiltered rtl bufNum bufLen iter = do
    readIter rtl bufNum bufLen
        (     EL.concatMap (map toC . ST.toList)
        =$= firdec
        =$= firdec
        =$= firdec
        =$= firdec
        =$  iter
        )
    where
        firdec = lfilter fir2 (U.singleton 1) =$= decimate 2

db :: C -> Float
db (a :+ b) = 10 * logBase 10 (a*a + b*b)

-- all of these could be implemented without the extra IO layer, but in order
-- to share state between invocations they need to be this way

avgPwr :: Float -> Enumeratee C Float IO b
avgPwr alpha = 
    EL.map db
    
    -- noise floor:
    -- y[m] = alpha * x[m] + (1 - alpha)*y[m-1]
    =$= lfilter
        (U.fromList [1,   alpha])
        (U.fromList [1, 1-alpha])

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
                (triggerIntervals ((> threshold) . db) =$ interp)
                -- (indexedE =$ dumpE)

main = test
