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
import Data.Maybe
import Data.Time
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word
import OOK
import qualified RTLSDR
import Samples
import System.IO
import Text.Printf

center, sampling :: Num a => a
center      = 433846000
sampling    = 250000

samples :: Num a => a -> a
samples sec = sec * sampling

data Packet a
    = AssocPulse      !(Token a)
    | Packet          !(Token a) !(U.Vector Word8)
    | InvalidPacket   !(Token a) !(U.Vector Word8)
    | InvalidWaveform  !(V.Vector (Token a))
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

packetize :: (Monad m, Ord a, Fractional a) => Enumeratee (Token a) (Packet a) m b
packetize = EL.concatMapAccum f id
    where
        inRange t0 t1 x = t0 <= x && x <= t1
        
        packDelim t = idle t || inRange 2.8e-3 4.5e-3 (duration sampling (delaySamples t))
        
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
                    | inRange 0.8e-3 1.2e-3 (duration sampling (delaySamples t)) = Just 0
                    | inRange 1.8e-3 2.2e-3 (duration sampling (delaySamples t)) = Just 1
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
dumpTokens = EL.mapM_ (putStrLn . pprToken sampling)

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
        (1e-6 * (center + realToFrac (sampleFreq sampling (pulseSamples t))) :: Float)
        count
    -- ++ " " ++ pprToken sampling t

printTimeStamped str = do
    now <- getCurrentTime
    putStrLn $ unwords ["[" ++ show now ++ "]", str]

-- supported gains for my particular device (unit: 0.1 dB):
-- [0,9,14,27,37,77,87,125,144,157,166,197,207,229,254,280,297,328,338,364,372,386,402,421,434,439,445,480,496]
gain        = Nothing
thresholdLo = 9
thresholdHi = 13
minPulse    = 50e-6
maxDelay    = 500e-3

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
               =$= dcReject (recip sampling)
               =$= lfilter fir (U.fromList [1])
               =$= tokenize 3.5e-5 thresholdLo thresholdHi (round (samples minPulse)) (round (samples maxDelay))
               =$= packetize
               =$= clusterMessages
               =$= EL.filter (knownMessage . snd)
               =$= EL.map (uncurry pprMessage)
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