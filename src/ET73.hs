{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{- 
    et73
    
    first iteration of a tool to receive and decode the signal from an
    older wireless dual-probe grill thermometer (Maverick ET-73)
 -}
module Main where

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Data.Bits
import Data.Monoid
import Data.Complex
import Data.Enumerator ((=$=), (=$), Iteratee(..), Enumeratee, Step(..), Stream(..))
import qualified Data.Enumerator.List as EL
import Data.IORef
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Storable as ST
import qualified Data.Vector.Unboxed as U
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import qualified RTLSDR
import Stats
import System.IO
import Text.Printf

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
toC (IQ i q) = (fromIntegral i - 128) :+ (fromIntegral q - 128)

center, sampling :: Fractional a => a
center      = 433.846e6
sampling    = 250e3

samples :: Fractional a => a -> a
samples sec = sec * sampling

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
        z_t = U.tail z `U.snoc` 0
        
        y   = (b_0 * x + z_0) / a_0
        
        updateZ a b z = b * x + z - a * y
     in (U.zipWith3 updateZ a_t b_t z_t, y)
    where
        n = max (U.length a') (U.length b')
        
        pad v = U.generate n $ \i ->
            if i < U.length v 
                then v U.! i
                else 0
        
        a_0 = U.head      a'
        a_t = U.tail (pad a')
        
        b_0 = U.head      b'
        b_t = U.tail (pad b')

-- discard out all but one of every 'n' samples
decimate :: Monad m => Int -> Enumeratee a a m b
decimate n = flip EL.concatMapAccum n $ \i x ->
    if i == n
        then (1,     [x])
        else (i + 1, [ ])

-- linear interpolation: "lerp x y" maps [0,1] to [x,y]
lerp x y alpha = alpha * y + (1 - alpha) * x

dcReject :: (Fractional a, Monad m) => a -> Enumeratee a a m b
dcReject alpha = flip EL.mapAccum 0 $ \dc x ->
    (lerp dc x alpha, x - dc)

magSq :: Num a => Complex a -> a
magSq (a :+ b) = a*a + b*b

db :: Floating a => a -> a
db z = (10 / log 10) * log z

pow :: Floating a => Complex a -> a
pow = db . magSq

indexed :: Monad m => Enumeratee a (Integer, a) m b
indexed = flip EL.mapAccum 0 $ \i x -> (i+1,(i,x))

data Samples a = Samples
    { magnitudeStats :: !(Stats a)
    , phaseStats     :: !(Stats a)
    }
    deriving (Eq, Ord, Read, Show)

instance (Ord a, Fractional a) => Monoid (Samples a) where
    mempty  = Samples mempty mempty
    mappend (Samples xs1 ps1) (Samples xs2 ps2) = 
        Samples (mappend xs1 xs2) (mappend ps1 ps2)

sample       x p = Samples (stats x) (stats p)
addSample ss x p = mappend ss (sample x p)

nSamples     = count . magnitudeStats
sampleMean   = mean . magnitudeStats

duration   t = fromIntegral (nSamples t) / sampling
sampleFreq t = sampling * mean (phaseStats t)

data TokenizerState = Pulse | Delay | Idle

data Token a = Token {pulseSamples :: !(Samples a), delaySamples :: !(Samples a), idle :: !Bool}
    deriving (Eq, Ord, Read, Show)

tokenSNR t = sampleMean (pulseSamples t) - sampleMean (delaySamples t)

debounce :: (Monad m, Ord a, Fractional a) => Int -> Enumeratee (Token a) (Token a) m b
debounce minPulse = EL.concatMapAccum accum Nothing
    where
        short t = (nSamples (pulseSamples t) < minPulse)
        extend (Token p0 d0 i0) (Token p1 d1 i1) =
            Token p0 (mconcat [d0, p1, d1]) (i0 || i1)
        
        accum Nothing t
            | short t   = (Nothing, [])
            | idle t    = (Nothing, [t])
            | otherwise = (Just t, [])
        accum (Just prev) t
            | short t   = (Just (extend prev t), [])
            | idle t    = (Nothing, [prev, t])
            | otherwise = (Just t, [prev])
        

tokenize :: (Monad m, Ord a, RealFloat a) => a -> a -> a -> Int -> Int -> Enumeratee (Complex a) (Token a) m b
tokenize alpha thresholdLo thresholdHi minPulse maxDelay = 
        EL.mapAccum snr (0/0, 0/0)
    =$= EL.concatMapAccum f (Idle, mempty, mempty)
    =$= debounce minPulse
    where
        snr (prev, nf) z = 
            let p       = db (magSq z)
                nf' | isInfinite nf
                    || isNaN nf     = p
                    | z == 0        = nf
                    | otherwise     = lerp nf p alpha
                
             in ((z, nf'), (p - nf, phase (z / prev)))
        
        f t@(Idle, ps, ds) (x, p)
            | x > thresholdHi           = ((Pulse, sample x p, ds),         [])
            | otherwise                 = (t,                               [])
        
        f t@(Delay, ps, ds) (x, p)
            | x > thresholdHi           = ((Pulse, sample x p, mempty),     [Token ps ds False])
            | nSamples ds > maxDelay    = ((Idle, mempty, mempty),          [Token ps ds True])
            | otherwise                 = ((Delay, ps, addSample ds x p),   [])
        
        f t@(Pulse, ps, ds) (x, p)
            | x > thresholdLo           = ((Pulse, addSample ps x p, ds),   [])
            | otherwise                 = ((Delay, ps, sample x p),         [])

inRange t0 t1 x = t0 <= x && x <= t1

data Packet a
    = AssocPulse      !(Token a)
    | Packet          !(Token a) !(U.Vector Word8)
    | InvalidPacket   !(Token a) !(U.Vector Word8)
    | InvalidWaveform  !(V.Vector (Token a))
    deriving (Eq, Ord, Read, Show)

pprPacket (AssocPulse t) = "AssocPulse: " ++ pprToken t
pprPacket (Packet _ v) = "Packet: " ++ show (U.toList v)
pprPacket (InvalidPacket _ v) = "InvalidPacket: " ++ show (U.toList v)
pprPacket (InvalidWaveform v) = "InvalidWaveform:\n" ++ unlines
    [ "   " ++ pprToken t
    | t <- V.toList v
    ]

validWaveform InvalidWaveform{} = False
validWaveform _                 = True

validPacket InvalidWaveform{}   = False
validPacket InvalidPacket{}     = False
validPacket _                   = True

packetSNR (AssocPulse t)        = tokenSNR t
packetSNR (Packet        t _)   = tokenSNR t
packetSNR (InvalidPacket t _)   = tokenSNR t
packetSNR (InvalidWaveform v)   = V.maximum (V.map tokenSNR v)

summarizeTokens :: (Ord a, Fractional a) => [Token a] -> Token a
summarizeTokens = foldl' f (Token mempty mempty False)
    where
        f (Token ps0 ds0 i0) (Token ps1 ds1 i1) = 
            Token (mappend ps0 ps1) (mappend ds0 ds1) (i0 || i1)


packetize :: (Monad m, Ord a, Fractional a) => Enumeratee (Token a) (Packet a) m b
packetize = EL.concatMapAccum f id
    where
        packDelim t = idle t || inRange 2.8e-3 4.5e-3 (duration (delaySamples t))
        
        f p t
            | packDelim t   = (id,       [parsePacket t (p [])])
            | otherwise     = (p . (t:), [])

        parsePacket endToken tokens = 
            case validBits of
                Just bs@(_:_) -> maybe
                    (InvalidPacket ts  (U.fromList bs))
                    (Packet        ts . U.fromList)
                    (packBytes bs)
                _       -> InvalidWaveform (V.fromList tokens `V.snoc` endToken)
             where
                ts          = summarizeTokens (endToken : tokens)
                
                validBits   = mapM validBit tokens
                
                validBit t
                    | inRange 0.8e-3 1.2e-3 (duration (delaySamples t)) = Just 0
                    | inRange 1.8e-3 2.2e-3 (duration (delaySamples t)) = Just 1
                    | otherwise             = Nothing
                
                packByte bits = do
                    guard (length bits == 8)
                    return (foldl (\x y -> 2 * x + y) 0 bits)
                
                packBytes [] = pure []
                packBytes bits = (:)
                    <$> packByte  byte
                    <*> packBytes rest
                    where
                        (byte,rest) = splitAt 8 bits

{-

-- TODO: see if this version is worth saving...
-- as it is right now it's too eager to group stuff, blocks up the pipeline

bits :: Monad m => Enumeratee (Token a) (Either (Token a) (Token a, Word8)) m b
bits =  EL.map recognizeBit
    where
        recognizeBit t
            | bitPulse && zeroDelay = Right (t, 0)
            | bitPulse && oneDelay  = Right (t, 1)
            | otherwise             = Left t
            where
                pulse = duration (pulseSamples t)
                delay = duration (delaySamples t)
                
                bitPulse    = inRange 0.1e-3 0.3e-3 pulse
                zeroDelay   = inRange 0.8e-3 1.2e-3 delay
                oneDelay    = inRange 1.8e-3 2.2e-3 delay

collectEithers :: Monad m => Enumeratee (Either a b) (Either [a] [b]) m c
collectEithers = EL.concatMapAccum accum (Left id)
    where
        start = either (Left . (:)) (Right . (:))
        
        finish c f 
            | null xs   = [] 
            | otherwise = [c xs]
            where xs = f []
        
        accum (Left  f) (Left  x) = (Left  (f . (x:)), [])
        accum (Right f) (Right x) = (Right (f . (x:)), [])
        accum        f         x  = (start x, either (finish Left) (finish Right) f)

packetize :: (Monad m, Ord a, Fractional a) => Enumeratee (Token a) (Packet a) m b
packetize 
     =  bits
    =$= collectEithers
    =$= EL.map (either parseOtherWaveform parsePacket)
    where
        parseOtherWaveform [t]
            | inRange 25e-3 27e-3 (duration (pulseSamples t))
                = AssocPulse t
        parseOtherWaveform ts = InvalidWaveform (V.fromList ts)
        
        parsePacket chunk = case packBytes bs of
            Nothing     -> InvalidPacket (summarizeTokens ts) (U.fromList bs)
            Just bytes  -> Packet        (summarizeTokens ts) (U.fromList bytes)
            where
                (ts, bs) = unzip chunk
                
                packByte bits = do
                    guard (length bits == 8)
                    return (foldl (\x y -> 2 * x + y) 0 bits)
                
                packBytes [] = pure []
                packBytes bits = (:)
                    <$> packByte  byte
                    <*> packBytes rest
                    where
                        (byte,rest) = splitAt 8 bits

-}

clusterByE :: Monad m => (a -> Bool) -> (a -> a -> Bool) -> Enumeratee a [a] m b
clusterByE end eq = EL.concatMapAccum accum Nothing
    where
        accum Nothing x
            | end x     = (Nothing,                     [[x]])
            | otherwise = (Just (x, id),                [])
        
        accum (Just (prev, rest)) x
            | end x     = (Nothing,                     [rest [prev, x]])
            | eq prev x = (Just (x, rest . (prev :)),   [])
            | otherwise = (Just (x, id),                [rest [prev]])

clusterMessages :: (Ord a, Fractional a, Monad m) => Enumeratee (Packet a) (Int, Message a) m b
clusterMessages 
     =  EL.map interp
    =$= clusterByE endMessage eqMessages
    =$= EL.map summarize
    where
        endMessage (TemperatureReading t _ _ _ _ _) = idle t
        endMessage UnknownMessage{}                 = True
        
        summarize ms@(TemperatureReading _ a b c d e:_) = 
            ( length ms
            , TemperatureReading (summarizeTokens ts) a b c d e
            )
            where ts = [ t | TemperatureReading t _ _ _ _ _ <- ms]
        summarize ms = (length ms, last ms)


eqPackets (Packet _ a) (Packet _ b) = a == b
eqPackets a b = a == b

eqMessages (TemperatureReading _ a0 b0 c0 d0 e0)
           (TemperatureReading _ a1 b1 c1 d1 e1) =
        a0 == a1
     && b0 == b1
     && c0 == c1
     && d0 == d1
     && e0 == e1
     
eqMessages a b = a == b

dump :: Show a => Iteratee a IO ()
dump = EL.mapM_ print

dumpTokens :: Iteratee (Token Float) IO ()
dumpTokens = EL.mapM_ (putStrLn . pprToken)

pprToken :: Token Float -> String
pprToken t = printf "pulse: %6.1fµs, %5.1fdB, %8.1fkHz, delay: %6.1fµs, %5.1fdB, %8.1fkHz%s"
    (duration   (pulseSamples t) * 1e6  :: Float)
    (sampleMean (pulseSamples t))
    (sampleFreq (pulseSamples t) * 1e-3 :: Float)
    (duration   (delaySamples t) * 1e6  :: Float)
    (sampleMean (delaySamples t))
    (sampleFreq (delaySamples t) * 1e-3 :: Float)
    (if idle t then " (idle)" else "")

data Message a
    = TemperatureReading !(Token a) !Word8 !Int16 !Int16 !Word8 !Word8
    | UnknownMessage !(Packet a)
    deriving (Eq, Ord, Read, Show)

knownMessage UnknownMessage{}   = False
knownMessage _                  = True

messageSNR :: (Ord a, Num a) => Message a -> a
messageSNR (TemperatureReading t _ _ _ _ _)    = tokenSNR t
messageSNR (UnknownMessage p)                  = packetSNR p

interp :: (Ord a, Num a) => Packet a -> Message a
interp (Packet t bs)
    | U.length bs == 6  = TemperatureReading t probe (s12 t0) (s12 t1) flags cksum
        where
            [a, b, c, d, e, f] = U.toList bs
            
            -- sign-extend 12 bits to 16 bits
            s12 :: Word16 -> Int16
            s12 x 
                | x .&. 0x0800  == 0    = fromIntegral  x
                | otherwise             = fromIntegral (x .|. 0xf000)
            
            c0      = (c .&. 0xf0) `shiftR` 4
            c1      = (c .&. 0x0f) `shiftL` 4
            
            probe   = a
            t0      = fromIntegral b  * 16 + fromIntegral c0
            t1      = fromIntegral c1 * 16 + fromIntegral d
            flags   = e
            cksum   = sum 
                [ a, e, f
                , fromIntegral t0, 0xf .&. fromIntegral (t0 `shiftR` 8)
                , fromIntegral t1, 0xf .&. fromIntegral (t1 `shiftR` 8)
                ]
interp p = UnknownMessage p

degC, degF :: Int16 -> Float
degC x = fromIntegral x / 10
degF x = 1.8 * degC x + 32

pprTemp :: Int16 -> Bool -> String
pprTemp x False = printf "-- no probe --"
pprTemp x True  = printf "%5.1fºC/%5.1fºF" (degC x) (degF x)

pprMessage :: Int -> Message Float -> String
pprMessage _ (UnknownMessage p) = pprPacket p
pprMessage count (TemperatureReading t p t0 t1 flags cksum)
    = printf "probe 0x%2x: %s %s [flags: %02x, checksum: %s, SNR: %5.1fdB, freq: %8.3fMHz count: %2d]"
        p
        (pprTemp t0 (testBit flags 6))
        (pprTemp t1 (testBit flags 7))
        flags
        (if cksum == 0 then "OK" else printf "failed (%02x)" cksum)
        (realToFrac (tokenSNR t) :: Float)
        (1e-6 * (center + realToFrac (sampleFreq (pulseSamples t))) :: Float)
        count
    -- ++ " " ++ pprToken t

-- supported gains for my particular device (unit: 0.1 dB):
-- [0,9,14,27,37,77,87,125,144,157,166,197,207,229,254,280,297,328,338,364,372,386,402,421,434,439,445,480,496]
gain        = Nothing
thresholdLo = 9
thresholdHi = 13
minPulse    = 50e-6
maxDelay    = 10e-3

main = do
    hSetBuffering stdout NoBuffering
    mbDev <- RTLSDR.open 0
    case mbDev of
        Nothing  -> return Nothing
        Just dev -> do
            RTLSDR.setSampleRate    dev (round sampling)
            RTLSDR.setCenterFreq    dev (round center)
            RTLSDR.setOffsetTuning  dev False
            RTLSDR.setAGCMode       dev (isNothing gain)
            RTLSDR.setTunerGainMode dev (isJust gain)
            maybe (return False) (RTLSDR.setTunerGain dev) gain
            RTLSDR.resetBuffer dev
            
            readIter dev 0 0
                (  EL.concatMap (map toC . ST.toList)
               =$= dcReject (recip sampling)
               =$= lfilter fir (U.fromList [1])
               =$= tokenize 3.5e-5 thresholdLo thresholdHi (round (samples minPulse)) (round (samples maxDelay))
               =$= EL.filter ((< 100e3) . abs . sampleFreq . pulseSamples)
               =$= packetize
               =$= clusterMessages
               =$= EL.filter (knownMessage . snd)
               =$  EL.mapM_ (putStrLn . uncurry pprMessage)
                )
    putStrLn "exiting"

fir :: U.Vector C
fir = U.fromList
    [ -0.05018194, -0.00856774,  0.00188394,  0.01599507,  0.0270292,   0.02820061
    ,  0.01633312, -0.00594583, -0.02987481, -0.04321448, -0.03488799,  0.00037788
    ,  0.05865464,  0.12703152,  0.1877356,   0.22327912,  0.22327912,  0.1877356
    ,  0.12703152,  0.05865464,  0.00037788, -0.03488799, -0.04321448, -0.02987481
    , -0.00594583,  0.01633312,  0.02820061,  0.0270292,   0.01599507,  0.00188394
    , -0.00856774, -0.05018194]