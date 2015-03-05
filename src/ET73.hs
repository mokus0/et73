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
import Control.Monad
import Data.Bits
import Data.Complex
import Data.Enumerator ((=$=), (=$), Iteratee, Enumeratee)
import qualified Data.Enumerator.List as EL
import Data.Enumerator.RTLSDR
import Data.Enumerator.Signal
import Data.Int
import qualified Data.IntMap as I
import Data.Maybe
import Data.Monoid
import Data.Time
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word
import OOK
import qualified RTLSDR
import Samples
import System.IO
import System.Locale
import Text.Printf

center, sampling :: Num a => a
center      = 433846000
sampling    = 250000

samples :: Num a => a -> a
samples sec = sec * sampling

data Packet a
    = AssocPulse        !(Token a)
    | PacketDelimiter   !(Token a)
    | Packet            !(Token a) !(U.Vector Word8)
    | InvalidPacket     !(Token a) !(U.Vector Word8)
    | InvalidWaveform   !(V.Vector (Token a))
    deriving (Eq, Ord, Read, Show)

pprPacket (AssocPulse t) = "AssocPulse: " ++ pprToken sampling t
pprPacket (Packet _ v) = "Packet: " ++ show (U.toList v)
pprPacket (InvalidPacket _ v) = "InvalidPacket: " ++ show (U.toList v)
pprPacket (InvalidWaveform v) = "InvalidWaveform:\n" ++ unlines
    [ "   " ++ pprToken sampling t
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

packetize :: (Monoid a, Ord b, Monad m, Fractional b) =>
     (a -> b) -> Enumeratee (Maybe (Token a)) (Maybe (Packet a)) m t
packetize f = EL.concatMapAccum accum id
    where
        inRange t0 t1 x = t0 <= x && x <= t1
        
        packDelim t = f (delay t) >= 3.5e-3
        
        accum p Nothing     = case p [] of
            []      -> (id,         [Nothing])
            tokens  -> (id,         [Just (parsePacket Nothing tokens), Nothing])
        accum p (Just t)
            | packDelim t   = (id,       [Just (parsePacket (Just t) (p []))])
            | otherwise     = (p . (t:), [])
        
        parsePacket mbEndToken tokens = 
            case mapM validBit tokens of
                Just bs@(_:_) -> maybe
                    (InvalidPacket ts  (U.fromList bs))
                    (Packet        ts . U.fromList)
                    (packBytes bs)
                _       -> InvalidWaveform (maybe id (flip V.snoc) mbEndToken (V.fromList tokens))
             where
                ts          = mconcat tokens
                
                validBit t
                    | inRange 0.8e-3 1.2e-3 (f (delay t))   = Just 0
                    | inRange 1.8e-3 2.2e-3 (f (delay t))   = Just 1
                    | otherwise                             = Nothing
                
                packByte bits = do
                    guard (length bits == 8)
                    return (foldl (\x y -> 2 * x + y) 0 bits)
                
                packBytes [] = pure []
                packBytes bits = (:)
                    <$> packByte  byte
                    <*> packBytes rest
                    where
                        (byte,rest) = splitAt 8 bits

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

clusterMessages :: (Ord a, Fractional a, Monad m) => Enumeratee (Maybe (Packet (Samples a))) (Int, Maybe (Message (Samples a))) m b
clusterMessages 
     =  EL.map (fmap interp)
    =$= clusterByE isNothing eq
    =$= EL.concatMap summarize
    where
        eq (Just (TempMessage _ m0)) (Just (TempMessage _ m1)) = m0 == m1
        eq a b = a == b
        
        summarize ms@(Just (TempMessage _ m) : _) = 
            [ ( length ms
              , Just (TempMessage (mconcat ts) m)
              )
            ]
            where ts = [ t | Just (TempMessage t _) <- ms]
        summarize ms = [(1, m) | m <- ms]

temperatureReadings :: Monad m => Enumeratee (Message a) TemperatureReading m b
temperatureReadings = EL.concatMap proj
    where
        proj (TempMessage _ m)  = [m]
        proj _                  = []

processMessages :: Monad m => Enumeratee TemperatureReading TemperatureReading m b
processMessages = EL.concatMapAccum onMessage I.empty
    where
        onMessage ms m = (ms', [m | mbOld /= Just m])
            where
                insertLookup = I.insertLookupWithKey (\_key new _old -> new)
                (mbOld, ms') = insertLookup (fromIntegral (probeID m)) m ms

dump :: Show a => Iteratee a IO ()
dump = EL.mapM_ print

dumpTokens :: Iteratee (Token (Samples Float)) IO ()
dumpTokens = EL.mapM_ (putStrLn . pprToken sampling)

data TemperatureReading = TemperatureReading
    { probeID   :: !Word8
    , temp1     :: !Int16
    , temp2     :: !Int16
    , flags     :: !Word8
    , cksum     :: !Word8
    }
    deriving (Eq, Ord, Read, Show)

data Message a
    = TempMessage !(Token a) !TemperatureReading
    | UnknownMessage !(Packet a)
    deriving (Eq, Ord, Read, Show)

knownMessage UnknownMessage{}   = False
knownMessage _                  = True

messageSNR :: (Ord a, Num a) => Message (Samples a) -> a
messageSNR (TempMessage t _)    = tokenSNR t
messageSNR (UnknownMessage p)   = packetSNR p

interp :: Packet a -> Message a
interp (Packet t bs)
    | U.length bs == 6  = TempMessage t (TemperatureReading probe (s12 t0) (s12 t1) flags cksum)
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

pprTemperatureReading :: TemperatureReading -> String
pprTemperatureReading (TemperatureReading p t0 t1 flags cksum)
    = printf "probe 0x%2x: %s %s [flags: %02x, checksum: %s]"
        p
        (pprTemp t0 (testBit flags 6))
        (pprTemp t1 (testBit flags 7))
        flags
        (if cksum == 0 then "OK" else printf "failed (%02x)" cksum)


pprMessage :: Int -> Message (Samples Float) -> String
pprMessage _ (UnknownMessage p) = pprPacket p
pprMessage count (TempMessage t m)
    = printf "%s [SNR: %5.1fdB, freq: %8.3fMHz count: %2d]"
        (pprTemperatureReading m)
        (realToFrac (tokenSNR t) :: Float)
        (1e-6 * (center + realToFrac (sampleFreq sampling (pulse t))) :: Float)
        count
    -- ++ " " ++ pprToken sampling t

printTimeStamped str = do
    now <- getCurrentTime
    putStrLn $ unwords ["[" ++ formatTime defaultTimeLocale "%F %T %Z" now ++ "]", str]

-- supported gains for my particular device (unit: 0.1 dB):
-- [0,9,14,27,37,77,87,125,144,157,166,197,207,229,254,280,297,328,338,364,372,386,402,421,434,439,445,480,496]
gain        = Just 297
thresholdLo = 10
thresholdHi = 14
minPulse    = 50e-6
maxDelay    = 100e-3
alpha       = 3.5e-5

main = do
    hSetBuffering stdout NoBuffering
    mbDev <- RTLSDR.open 0
    case mbDev of
        Nothing  -> return Nothing
        Just dev -> do
            RTLSDR.setSampleRate    dev sampling
            RTLSDR.setCenterFreq    dev center
            RTLSDR.setOffsetTuning  dev False
            RTLSDR.setAGCMode       dev (isNothing gain)
            RTLSDR.setTunerGainMode dev (isJust gain)
            maybe (return False) (RTLSDR.setTunerGain dev) gain
            RTLSDR.resetBuffer dev
            
            readIter dev 0 0
                (  EL.concatMap U.toList
               =$= runFIR fir
               =$= dcReject (recip sampling)
               
               -- =$= toSamples alpha
               -- =$= tokenizeSamples thresholdLo thresholdHi (round (samples minPulse)) (round (samples maxDelay))
               -- -- =$= EL.map (maybe "" (pprToken sampling))
               -- =$= packetize (duration sampling)
               -- =$= EL.map (maybe "" pprPacket)
               -- -- =$= clusterMessages
               -- -- =$= EL.concatMap (\(n, mbM) -> case mbM of {Nothing -> []; Just m -> [(n, m)]})
               -- -- =$= EL.filter (knownMessage . snd)
               -- -- =$= EL.map (uncurry pprMessage)
               
               =$= snr alpha
               =$= tokenizeCount thresholdLo thresholdHi (round (samples minPulse)) (round (samples maxDelay))
               =$= packetize ((/ sampling) . fromIntegral . getSum)
               =$= EL.map (fmap interp)
               =$= EL.concatMap (maybe [] (:[]))
               =$= temperatureReadings
               =$= processMessages
               =$= EL.map pprTemperatureReading
               
               =$  EL.mapM_ printTimeStamped
                )
    putStrLn "exiting"

fir :: U.Vector (Complex Float)
fir = U.fromList
    [ -0.05018194, -0.00856774,  0.00188394,  0.01599507,  0.0270292,   0.02820061
    ,  0.01633312, -0.00594583, -0.02987481, -0.04321448, -0.03488799,  0.00037788
    ,  0.05865464,  0.12703152,  0.1877356,   0.22327912,  0.22327912,  0.1877356
    ,  0.12703152,  0.05865464,  0.00037788, -0.03488799, -0.04321448, -0.02987481
    , -0.00594583,  0.01633312,  0.02820061,  0.0270292,   0.01599507,  0.00188394
    , -0.00856774, -0.05018194]