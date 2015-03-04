module OOK
    ( magSq
    , db
      
    , Token
    , pulseSamples
    , delaySamples
    , idle
    , tokenSNR
    , summarizeTokens
    , pprToken
    
    , tokenize
    ) where

import Data.Monoid
import Data.Complex
import Data.Enumerator ((=$=), Enumeratee)
import qualified Data.Enumerator.List as EL
import Data.Enumerator.Signal
import Data.List
import Samples
import Text.Printf

magSq :: Num a => Complex a -> a
magSq (a :+ b) = a*a + b*b

db :: Floating a => a -> a
db z = (10 / log 10) * log z

data Token a = Token {pulseSamples :: !(Samples a), delaySamples :: !(Samples a), idle :: !Bool}
    deriving (Eq, Ord, Read, Show)

tokenSNR :: Num a => Token a -> a
tokenSNR t = sampleMean (pulseSamples t) - sampleMean (delaySamples t)

summarizeTokens :: (Ord a, Fractional a) => [Token a] -> Token a
summarizeTokens = foldl' f (Token mempty mempty False)
    where
        f (Token ps0 ds0 i0) (Token ps1 ds1 i1) = 
            Token (mappend ps0 ps1) (mappend ds0 ds1) (i0 || i1)

pprToken :: Float -> Token Float -> String
pprToken rate t = printf "pulse: %6.1fµs, %5.1fdB, %8.1fkHz, delay: %6.1fµs, %5.1fdB, %8.1fkHz%s"
    (duration rate   (pulseSamples t) * 1e6  :: Float)
    (sampleMean (pulseSamples t))
    (sampleFreq rate (pulseSamples t) * 1e-3 :: Float)
    (duration rate   (delaySamples t) * 1e6  :: Float)
    (sampleMean (delaySamples t))
    (sampleFreq rate (delaySamples t) * 1e-3 :: Float)
    (if idle t then " (idle)" else "")

data TokenizerState = Pulse | Delay | Idle

-- for each sample, calculate signal-to-noise ratio (with
-- alpha as exponential filter parameter) and phase change
-- relative to previous sample
{-# INLINE sampleize #-}
sampleize alpha = EL.mapAccum nextSample (0/0, 0/0)
    where
        noiseFloor prev p
            | isInfinite prev
            || isNaN prev       = p
            | otherwise         = lerp prev p alpha
        
        phaseChange z0 z1 = phase (z1 / z0)
        
        nextSample (prevZ, prevNF) z = 
            let p       = db (magSq z)
                nf      = if z == 0
                    then prevNF
                    else noiseFloor prevNF p
                
             in ((z, nf), (p - prevNF, phaseChange prevZ z))

{-# INLINE tokenize #-}
tokenize :: (Monad m, Ord a, RealFloat a) => a -> a -> a -> Int -> Int -> Enumeratee (Complex a) (Token a) m b
tokenize alpha thresholdLo thresholdHi minPulse maxDelay =
        sampleize alpha
    =$= EL.concatMapAccum f (Idle, mempty, mempty)
    =$= debounce minPulse
    where
        f t@(Idle, ps, ds) ~(x, p)
            | x > thresholdHi           = ((Pulse, sample x p, ds),         [])
            | otherwise                 = (t,                               [])
        
        f t@(Delay, ps, ds) ~(x, p)
            | x > thresholdHi           = ((Pulse, sample x p, mempty),     [Token ps ds False])
            | nSamples ds > maxDelay    = ((Idle, mempty, mempty),          [Token ps ds True])
            | otherwise                 = ((Delay, ps, addSample ds x p),   [])
        
        f t@(Pulse, ps, ds) ~(x, p)
            | x > thresholdLo           = ((Pulse, addSample ps x p, ds),   [])
            | otherwise                 = ((Delay, ps, sample x p),         [])

{-# INLINE debounce #-}
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
