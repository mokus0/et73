{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{- 
    mt73
    
    first iteration of a tool to receive and decode the signal from an
    older wireless dual-probe grill thermometer (Maverick ET-73)
    
    also my first real attempt to make use of the 'conduit' library.
    it doesn't seem all that well suited for this application.
 -}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Complex
import Data.Conduit
import Data.IORef
import qualified Data.Vector.Storable as ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word
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

readSink :: RTLSDR.RTLSDR -> Word32 -> Word32 -> Sink (ST.Vector IQ) IO () -> IO Bool
readSink rtl bufNum bufLen sink = RTLSDR.readAsync rtl bufNum bufLen callback
    where
        callback p_buf len = bufSource p_buf len $$ sink
        
        bufSource p_buf len = do
            --liftIO $ putStrLn $ unwords
            --    [ "len: "
            --    ,  show len
            --    ]
            
            let elems = len `div` sizeOf (IQ 0 0)
            
            fp_copy <- liftIO (mallocForeignPtrArray elems)
            liftIO $ withForeignPtr fp_copy $ \p_copy ->
                copyArray p_copy (castPtr p_buf) elems
            yield $! ST.unsafeFromForeignPtr fp_copy 0 elems

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

lfilter :: (Fractional t, Eq t, U.Unbox t) => U.Vector t -> U.Vector t -> IO (Conduit t IO t)
lfilter b a = do
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
    
    return $ awaitForever $ \x -> do
            z_0 <- liftIO (MU.read z 0)
            let y = (b_0 * x + z_0) / a_0
            yield y
            
            liftIO $ do
                let bx_m_ay = U.zipWith (-) (U.map (* x) b_t)
                                            (U.map (* y) a_t)
                
                sequence_
                    [ do
                        z_ip1 <- MU.read z (i + 1)
                        MU.write z i (z_ip1 + (bx_m_ay U.! i))
                    | i <- [0 .. n-3]
                    ]
                
                MU.write z (n-2) (bx_m_ay U.! (n-2))

decimate :: Monad m => Int -> Conduit a m a
decimate n = await >>= loop
    where
        loop Nothing = return ()
        loop (Just x) = do
            yield x
            replicateM_ (n-1) await
            await >>= loop

readFiltered :: RTLSDR.RTLSDR -> Word32 -> Word32 -> Sink C IO () -> IO Bool
readFiltered rtl bufNum bufLen sink = do
    --filter1Conduit <- lfilter fir (U.singleton 1)
    --filter2Conduit <- lfilter fir (U.singleton 1)
    filter1Conduit <- lfilter fir2 (U.singleton 1)
    filter2Conduit <- lfilter fir2 (U.singleton 1)
    filter3Conduit <- lfilter fir2 (U.singleton 1)
    filter4Conduit <- lfilter fir2 (U.singleton 1)
    
    readSink rtl bufNum bufLen
        (  awaitForever (ST.mapM_ (yield . toC))
       -- =$= filter1Conduit
       -- =$= decimate 5
       -- =$= filter2Conduit
       -- =$= decimate 5
       =$= filter1Conduit
       =$= decimate 2
       =$= filter2Conduit
       =$= decimate 2
       =$= filter3Conduit
       =$= decimate 2
       =$= filter4Conduit
       =$= decimate 2
       =$= sink
        )

db :: C -> Float
db (a :+ b) = 10 * logBase 10 (a*a + b*b)

-- all of these could be implemented without the extra IO layer, but in order
-- to share state between invocations they need to be this way

avgPwr :: IO (Conduit C IO Float)
avgPwr = do
    -- noise floor:
    -- y[m] = alpha * x[m] + (1 - alpha)*y[m-1]
    
    let alpha = 0.1
    avgConduit <- lfilter
        (U.fromList [1,   alpha])
        (U.fromList [1, 1-alpha])
    
    return (avgConduit =$= awaitForever (yield . db))

indexed :: IO (Conduit a IO (Integer, a))
indexed = do
    r_i <- newIORef 0
    
    return $ awaitForever $ \a -> do
        i <- liftIO $ atomicModifyIORef' r_i $ \i -> (i + 1, i)
        yield (i, a)
        

trigger :: (a -> Bool) -> IO (Conduit a IO a)
trigger p = do
    r_fired <- newIORef False
    
    return $ awaitForever $ \x -> do
        let fire = p x
        
        when fire $ do
            fired <- liftIO (readIORef r_fired)
            when (not fired) (yield x)
        
        liftIO (writeIORef r_fired fire)

trigger_ :: (a -> Bool) -> IO (Conduit a IO Integer)
trigger_ p = mapOutput fst <$>
    ((=$=) <$> indexed <*> trigger (p . snd))

diff :: Num a => IO (Conduit a IO a)
diff = do
    r_prev <- newIORef undefined
    
    return $ do
        mbFirst <- await
        case mbFirst of
            Nothing     -> return ()
            Just first  -> do
                liftIO (writeIORef r_prev first)
                awaitForever $ \next -> do
                    d <- liftIO (atomicModifyIORef' r_prev $ \prev -> (next, next - prev))
                    yield d

triggerIntervals :: (a -> Bool) -> IO (Conduit a IO Integer)
triggerIntervals p = (=$=) <$> trigger_ p <*> diff

dump :: Show a => Sink a IO ()
dump = awaitForever (liftIO . print)

interp :: Sink Integer IO ()
interp = awaitForever $ \dt ->
    liftIO $ putChar $
        if dt > 40 then '\n'
            else if dt > 22 then '1'
                else '0'

test = do
    mbDev <- RTLSDR.open 0
    case mbDev of
        Nothing  -> return False
        Just dev -> do
            RTLSDR.setSampleRate dev (round sampling)
            RTLSDR.setCenterFreq dev (round center)
            RTLSDR.setAGCMode dev False
            RTLSDR.setTunerGainMode dev True
            RTLSDR.setTunerGain dev 0
            RTLSDR.resetBuffer dev
            
            triggerConduit <- triggerIntervals ((> 0) . db)
            readFiltered dev 0 0 -- (awaitForever (\(a :+ b) -> liftIO (printf "%f\t%f\n" a b)))
                (triggerConduit =$= interp)
                --(avgPwrConduit =$= dump)
                --(mapInput db (Just . (:+ 0)) dump)

main = test
