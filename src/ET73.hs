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
import Data.Complex
import Data.Enumerator ((=$=), (=$), Iteratee(..), Enumeratee, Step(..), Stream(..))
import qualified Data.Enumerator.List as EL
import Data.IORef
import Data.Int
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
toC (IQ i q) = (fromIntegral i - 127) :+ (fromIntegral q - 127)

center, sampling :: Fractional a => a
center      = 433.846e6
sampling    = 250e3

decimation :: Num a => a
decimation  = 10

samples :: Fractional a => a -> a
samples sec = sec * sampling / decimation

fir :: U.Vector C
fir = U.fromList
    [ -9.30242857e-05, -1.17815322e-04, -1.37578498e-04, -1.49997562e-04
    , -1.53044578e-04, -1.45180040e-04, -1.25534707e-04, -9.40566127e-05
    , -5.16080549e-05,  1.07912788e-18,  5.80447912e-05,  1.19003748e-04
    ,  1.78739846e-04,  2.32752717e-04,  2.76482123e-04,  3.05645213e-04
    ,  3.16584811e-04,  3.06603216e-04,  2.74254961e-04,  2.19572921e-04
    ,  1.44205040e-04,  5.14438518e-05, -5.38624550e-05, -1.65520771e-04
    , -2.76320131e-04, -3.78460634e-04, -4.64061105e-04, -5.25714807e-04
    , -5.57056664e-04, -5.53301842e-04, -5.11714582e-04, -4.31968200e-04
    , -3.16362111e-04, -1.69869564e-04,  7.82714377e-19,  1.83527931e-04
    ,  3.69293405e-04,  5.44837752e-04,  6.97450551e-04,  8.15030838e-04
    ,  8.86968277e-04,  9.04983941e-04,  8.63869099e-04,  7.62063428e-04
    ,  6.02021405e-04,  3.90327023e-04,  1.37531961e-04, -1.42290087e-04
    , -4.32260934e-04, -7.13655670e-04, -9.67050428e-04, -1.17359360e-03
    , -1.31632126e-03, -1.38142949e-03, -1.35941396e-03, -1.24599064e-03
    , -1.04272118e-03, -7.57282113e-04, -4.03337575e-04,  1.43420392e-18
    ,  4.29109954e-04,  8.57164825e-04,  1.25572705e-03,  1.59655096e-03
    ,  1.85348094e-03,  2.00432390e-03,  2.03256881e-03,  1.92882907e-03
    ,  1.69189502e-03,  1.32930360e-03,  8.57359323e-04,  3.00573509e-04
    , -3.09474052e-04, -9.35808373e-04, -1.53818670e-03, -2.07556482e-03
    , -2.50875800e-03, -2.80313176e-03, -2.93114631e-03, -2.87457908e-03
    , -2.62626077e-03, -2.19118422e-03, -1.58687823e-03, -8.42981457e-04
    ,  2.13591852e-18,  8.92715120e-04,  1.77968160e-03,  2.60255190e-03
    ,  3.30374986e-03,  3.83026208e-03,  4.13734725e-03,  4.19191909e-03
    ,  3.97536669e-03,  3.48560017e-03,  2.73814742e-03,  1.76618004e-03
    ,  6.19407830e-04, -6.38149398e-04, -1.93143266e-03, -3.17850668e-03
    , -4.29540034e-03, -5.20135619e-03, -5.82418110e-03, -6.10536766e-03
    , -6.00465313e-03, -5.50370064e-03, -4.60862487e-03, -3.35114201e-03
    , -1.78819609e-03,  2.73201377e-18,  1.91347801e-03,  3.83745546e-03
    ,  5.64848575e-03,  7.22151516e-03,  8.43747622e-03,  9.19099681e-03
    ,  9.39777819e-03,  9.00119280e-03,  7.97767801e-03,  6.34055212e-03
    ,  4.14195326e-03,  1.47269629e-03, -1.54004727e-03, -4.73722212e-03
    , -7.93436814e-03, -1.09298407e-02, -1.35144003e-02, -1.54817098e-02
    , -1.66392106e-02, -1.68188104e-02, -1.58868058e-02, -1.37524863e-02
    , -1.03749209e-02, -5.76751096e-03,  3.07481851e-18,  6.80223924e-03
    ,  1.44616829e-02,  2.27547950e-02,  3.14205826e-02,  4.01711885e-02
    ,  4.87040333e-02,  5.67149260e-02,  6.39115072e-02,  7.00263609e-02
    ,  7.48291412e-02,  7.81371057e-02,  7.98235219e-02,  7.98235219e-02
    ,  7.81371057e-02,  7.48291412e-02,  7.00263609e-02,  6.39115072e-02
    ,  5.67149260e-02,  4.87040333e-02,  4.01711885e-02,  3.14205826e-02
    ,  2.27547950e-02,  1.44616829e-02,  6.80223924e-03,  3.07481851e-18
    , -5.76751096e-03, -1.03749209e-02, -1.37524863e-02, -1.58868058e-02
    , -1.68188104e-02, -1.66392106e-02, -1.54817098e-02, -1.35144003e-02
    , -1.09298407e-02, -7.93436814e-03, -4.73722212e-03, -1.54004727e-03
    ,  1.47269629e-03,  4.14195326e-03,  6.34055212e-03,  7.97767801e-03
    ,  9.00119280e-03,  9.39777819e-03,  9.19099681e-03,  8.43747622e-03
    ,  7.22151516e-03,  5.64848575e-03,  3.83745546e-03,  1.91347801e-03
    ,  2.73201377e-18, -1.78819609e-03, -3.35114201e-03, -4.60862487e-03
    , -5.50370064e-03, -6.00465313e-03, -6.10536766e-03, -5.82418110e-03
    , -5.20135619e-03, -4.29540034e-03, -3.17850668e-03, -1.93143266e-03
    , -6.38149398e-04,  6.19407830e-04,  1.76618004e-03,  2.73814742e-03
    ,  3.48560017e-03,  3.97536669e-03,  4.19191909e-03,  4.13734725e-03
    ,  3.83026208e-03,  3.30374986e-03,  2.60255190e-03,  1.77968160e-03
    ,  8.92715120e-04,  2.13591852e-18, -8.42981457e-04, -1.58687823e-03
    , -2.19118422e-03, -2.62626077e-03, -2.87457908e-03, -2.93114631e-03
    , -2.80313176e-03, -2.50875800e-03, -2.07556482e-03, -1.53818670e-03
    , -9.35808373e-04, -3.09474052e-04,  3.00573509e-04,  8.57359323e-04
    ,  1.32930360e-03,  1.69189502e-03,  1.92882907e-03,  2.03256881e-03
    ,  2.00432390e-03,  1.85348094e-03,  1.59655096e-03,  1.25572705e-03
    ,  8.57164825e-04,  4.29109954e-04,  1.43420392e-18, -4.03337575e-04
    , -7.57282113e-04, -1.04272118e-03, -1.24599064e-03, -1.35941396e-03
    , -1.38142949e-03, -1.31632126e-03, -1.17359360e-03, -9.67050428e-04
    , -7.13655670e-04, -4.32260934e-04, -1.42290087e-04,  1.37531961e-04
    ,  3.90327023e-04,  6.02021405e-04,  7.62063428e-04,  8.63869099e-04
    ,  9.04983941e-04,  8.86968277e-04,  8.15030838e-04,  6.97450551e-04
    ,  5.44837752e-04,  3.69293405e-04,  1.83527931e-04,  7.82714377e-19
    , -1.69869564e-04, -3.16362111e-04, -4.31968200e-04, -5.11714582e-04
    , -5.53301842e-04, -5.57056664e-04, -5.25714807e-04, -4.64061105e-04
    , -3.78460634e-04, -2.76320131e-04, -1.65520771e-04, -5.38624550e-05
    ,  5.14438518e-05,  1.44205040e-04,  2.19572921e-04,  2.74254961e-04
    ,  3.06603216e-04,  3.16584811e-04,  3.05645213e-04,  2.76482123e-04
    ,  2.32752717e-04,  1.78739846e-04,  1.19003748e-04,  5.80447912e-05
    ,  1.07912788e-18, -5.16080549e-05, -9.40566127e-05, -1.25534707e-04
    , -1.45180040e-04, -1.53044578e-04, -1.49997562e-04, -1.37578498e-04
    , -1.17815322e-04, -9.30242857e-05]

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

readFiltered :: RTLSDR.RTLSDR -> Word32 -> Word32 -> Iteratee C IO a -> IO (Maybe a)
readFiltered rtl bufNum bufLen iter = do
    readIter rtl bufNum bufLen
        (  EL.concatMap (map toC . ST.toList)
       =$= dcReject (recip sampling)
       =$= lfilter fir (U.singleton 1)
       =$= decimate decimation
       =$  iter
        )

magSq :: Num a => Complex a -> a
magSq (a :+ b) = a*a + b*b

db :: Floating a => a -> a
db z = (10 / log 10) * log z

pow :: Floating a => Complex a -> a
pow = db . magSq

indexed :: Monad m => Enumeratee a (Integer, a) m b
indexed = flip EL.mapAccum 0 $ \i x -> (i+1,(i,x))

data Token a = Pulse !Int !a | Delay !Int !a | Idle !a
    deriving (Eq, Ord, Read, Show)

isPulse Pulse{} = True
isPulse _       = False

isDelay Delay{} = True
isDelay _       = False

isIdle  Idle{}  = True
isIdle  _       = False

tokenSNR (Pulse _ r) = r
tokenSNR (Delay _ r) = r
tokenSNR (Idle    r) = r

tokenize :: (Monad m, Ord a, Num a) => (a -> Bool) -> Int -> Enumeratee a (Token a) m b
tokenize p timeout = flip EL.concatMapAccum (Idle 0) f
    where
        f t@Idle{} x
            | p x           = (Pulse 1 x,                       [])
            | otherwise     = (t,                               [])
        
        f t@(Delay dt min_x) x
            | p x           = (Pulse 1 x,                       [t])
            | dt >= timeout = (Idle min_x,                      [Idle min_x])
            | otherwise     = (Delay (dt + 1) (min x min_x),    [])
        
        f t@(Pulse dt max_x) x
            | p x           = (Pulse (dt + 1) (max x max_x),    [])
            | otherwise     = (Delay 1 x,                       [t])

debounce :: (Monad m, Num a, Ord a) => Int -> Int -> Enumeratee (Token a) (Token a) m b
debounce np nd = flip EL.concatMapAccum (Idle 0) f
    where
        f Idle{} tok@(Pulse t _)
            | t < np            = (tok, [])
        f Idle{} tok@(Delay t _)
            | t < nd            = (tok, [])
        f Idle{} t              = (Idle 0, [t])
        
        f (Pulse t0 p0) (Delay t1 p1)
            | t < np                    = (Pulse t (max p0 p1), [])
            where t = t0 + t1
        f (Pulse t0 p0) (Pulse t1 p1)   = (Pulse t (max p0 p1), [])
            where t = t0 + t1
        
        f (Delay t0 p0) (Pulse t1 p1)
            | t < nd                    = (Delay t (min p0 p1), [])
            where t = t0 + t1
        f (Delay t0 p0) (Delay t1 p1)   = (Delay t (min p0 p1), [])
            where t = t0 + t1
        
        f acc t = (Idle 0, [acc, t])

data Packet a
    = AssocPulse      a
    | Packet          a !(Token a) !(U.Vector Word8)
    | InvalidPacket   a !(Token a) !(U.Vector Word8)
    | InvalidWaveform   !(V.Vector (Token a))
    deriving (Eq, Ord, Read, Show)

validWaveform InvalidWaveform{} = False
validWaveform _                 = True

validPacket InvalidWaveform{}   = False
validPacket InvalidPacket{}     = False
validPacket _                   = True

packetSNR (Packet r _ _)           = r
packetSNR (InvalidPacket r _ _)    = r
packetSNR (InvalidWaveform v)
    = V.minimum (V.map tokenSNR pulses)
    - V.maximum (V.map tokenSNR rest)
        where
            --mean xs = V.sum xs / fromIntegral (V.length xs)
            (pulses, rest) = V.partition isPulse v


packetize :: (Monad m, Ord a, Fractional a) => Enumeratee (Token a) (Packet a) m b
packetize = flip EL.concatMapAccum id f
    where
        inRange t0 t1 x =
            samples t0 <= fromIntegral x
            && fromIntegral x <= samples t1
        
        packDelim (Idle{})          = True
        packDelim (Delay dt _)  = inRange 2.8e-3 4.5e-3 dt
        packDelim _             = False
        
        f p t
            | packDelim t   = (id,       [parsePacket t (p [])])
            | otherwise     = (p . (t:), [])
        
        parsePacket endToken tokens = 
            case validBits of
                Just bs@(_:_) -> maybe
                    (InvalidPacket snr endToken  (U.fromList bs))
                    (Packet        snr endToken . U.fromList)
                    (packBytes bs)
                _       -> invalid
            where
                invalid     = InvalidWaveform (V.fromList tokens `V.snoc` endToken)
                snr         = packetSNR invalid
                
                validBits   = pairTokens tokens >>= mapM validBit
                
                pairTokens (Pulse p _ : Delay d _ : rest) = ((p,d):) <$> pairTokens rest
                pairTokens [Pulse _ _] = Just []
                pairTokens _  = Nothing
                
                validBit (p, d)
                    | inRange 1.0e-3 1.4e-3 (p + d) = Just 0
                    | inRange 2.0e-3 2.4e-3 (p + d) = Just 1
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

clusterMessages :: (Ord a, Fractional a, Monad m) => Enumeratee (Packet a) (Message a, [a]) m b
clusterMessages = EL.concatMapAccum f []
    where
        endToken (Packet        _ e _)  = e
        endToken (InvalidPacket _ e _)  = e
        endToken (InvalidWaveform v)    = V.last v
        
        isIdle Idle{}   = True
        isIdle _        = False
        
        f ms p = case interp p of
            m@UnknownMessage{}  -> ([], summarize ms ++ summarize [m])
            m1                  -> 
                case ms of
                    []              -> ([m1], [])
                    m0:_
                        | m0 `eqMessages` m1    -> 
                            if isIdle (endToken p)
                                then ([], summarize (ms ++ [m1]))
                                else (ms ++ [m1], [])
                        | otherwise             -> ([m1], summarize ms)
        
        summarize []        = []
        summarize ms@(m:_)   = [(m, map messageSNR ms)]


uniq :: Monad m => (a -> a -> Bool) -> Enumeratee a a m b
uniq p = EL.concatMapAccum f Nothing
    where
        f Nothing x = (Just x, [])
        f mbPrev@(Just prev) x
            | p prev x  = (mbPrev, [])
            | otherwise = (Just x, [x])

eqPackets (Packet _ _ a) (Packet _ _ b) = a == b
eqPackets a b = a == b

eqMessages (TemperatureReading _ a0 b0 c0 d0 e0)
           (TemperatureReading _ a1 b1 c1 d1 e1) =
        a0 == a1
     && b0 == b1
     && c0 == c1
     && d0 == d1
     && e0 == e1
     
eqMessages a b = a == b

snr :: (RealFloat a, Eq a, Monad m) => a -> Enumeratee (Complex a) (a, a) m b
snr alpha = flip EL.concatMapAccum (Left (0, 0/0)) f
    where
        sut     = round (recip alpha)
        
        -- startup phase; eat a certain number of
        -- initial samples to stabilize noise floor calculation
        f (Left (n, nf)) x
            | n < sut       = (Left (n+1, nf'), [ ])
            | otherwise     = (Right nf',       [y])
            where (!nf', y) = g nf x
        f (Right nf) x     = (Right nf',        [y])
            where (!nf', y) = g nf x
        
        g nf x = 
            let p       = pow x
                nf' | isInfinite nf
                    || isNaN nf     = p
                    | x == 0        = nf
                    | otherwise     = lerp nf p alpha
                
             in (nf', (nf, p - nf))

dump :: Show a => Iteratee a IO ()
dump = EL.mapM_ print

dumpIntervals :: (Fractional a, Ord a, Show a) => a -> Iteratee a IO ()
dumpIntervals timeout = EL.mapM_ $ \dt ->
    if dt >= samples timeout
        then print dt
        else putStr (show dt ++ " ")

dumpTokens :: (Show a, Ord a) => Iteratee (Token a) IO ()
dumpTokens = EL.mapM_ $ \t ->
    case t of
        Idle{}     -> putStrLn "" >> putStr "idle"
        Pulse dt _ -> putStr (',' : show dt)
        Delay dt _ -> putStr ('|' : show dt)

data Message a
    = TemperatureReading a !Word8 !Int16 !Int16 !Word8 !Word8
    | UnknownMessage !(Packet a)
    deriving (Eq, Ord, Read, Show)

knownMessage UnknownMessage{}   = False
knownMessage _                  = True

messageSNR :: (Ord a, Num a) => Message a -> a
messageSNR (TemperatureReading r _ _ _ _ _)    = r
messageSNR (UnknownMessage p)                  = packetSNR p

interp :: Packet a -> Message a
interp (Packet snr _ p)
    | U.length p == 6   = TemperatureReading snr probe (s12 t0) (s12 t1) flags cksum
        where
            [a, b, c, d, e, f] = U.toList p
            
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

pprMessage :: (Show a, RealFrac a) => Message a -> [a] -> String
pprMessage (UnknownMessage p) _ = show p
pprMessage (TemperatureReading _snr p t0 t1 flags cksum) snr
    = printf "probe 0x%2x: %s %s [flags: %02x, checksum: %s, SNR: %5.1fdB, count: %2d]"
        p
        (pprTemp t0 (testBit flags 6))
        (pprTemp t1 (testBit flags 7))
        flags
        (if cksum == 0 then "OK" else printf "failed (%02x)" cksum)
        (maximum (map realToFrac snr) :: Float)
        (length snr)

-- supported gains for my particular device (unit: 0.1 dB):
-- [0,9,14,27,37,77,87,125,144,157,166,197,207,229,254,280,297,328,338,364,372,386,402,421,434,439,445,480,496]
gain        = Nothing
threshold   = 15
ping z      = db (magSq z) > threshold
timeout     = 10e-3

main = do
    hSetBuffering stdout NoBuffering
    mbDev <- RTLSDR.open 0
    case mbDev of
        Nothing  -> return Nothing
        Just dev -> do
            RTLSDR.setSampleRate    dev (round sampling)
            RTLSDR.setCenterFreq    dev (round center)
            RTLSDR.setAGCMode       dev (isNothing gain)
            RTLSDR.setTunerGainMode dev (isJust gain)
            maybe (return False) (RTLSDR.setTunerGain dev) gain
            RTLSDR.resetBuffer dev
            
            readFiltered dev 0 0
                (  snr 3.5e-5 
               =$= EL.map snd
               =$= tokenize (> threshold) (round (samples timeout))
               =$= debounce 5 3
               =$= packetize
               =$= EL.filter validPacket
               =$= clusterMessages
               =$= EL.filter (knownMessage . fst)
               =$  EL.mapM_ (putStrLn . uncurry pprMessage)
                )

