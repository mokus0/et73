{-# LANGUAGE DeriveFunctor #-}
module OOK
    ( Token
    , pulse
    , delay
    , tokenSNR
    , pprToken
    
    , tokenizeBy
    , tokenizeCount
    , tokenizeSamples
    ) where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Enumerator ((=$=), Enumeratee)
import qualified Data.Enumerator.List as EL
import Samples
import Text.Printf

data Token a = Token {pulse :: !a, delay :: !a}
    deriving (Eq, Ord, Read, Show, Functor)

instance Monoid a => Monoid (Token a) where
    mempty = Token mempty mempty
    mappend (Token ps0 ds0) (Token ps1 ds1) = 
        Token (mappend ps0 ps1) (mappend ds0 ds1)

tokenSNR :: Num a => Token (Samples a) -> a
tokenSNR t = sampleMean (pulse t) - sampleMean (delay t)

pprToken :: Float -> Token (Samples Float) -> String
pprToken rate t = printf "pulse: %6.1fµs, %5.1fdB, %8.1fkHz, delay: %6.1fµs, %5.1fdB, %8.1fkHz"
    (duration rate   (pulse t) * 1e6  :: Float)
    (sampleMean (pulse t))
    (sampleFreq rate (pulse t) * 1e-3 :: Float)
    (duration rate   (delay t) * 1e6  :: Float)
    (sampleMean (delay t))
    (sampleFreq rate (delay t) * 1e-3 :: Float)

data TokenizerState a
    = Idle
    | Pulse !a
    | Delay !a !a

{-# INLINE tokenizeBy #-}
tokenizeBy :: (Monoid b, Monad m)
     => (a -> b)                -- inject samples into token
     -> (a -> Bool)             -- sample begins a pulse
     -> (a -> Bool)             -- sample ends a pulse
     -> (Token b -> Bool)       -- (possibly incomplete) token is too short and should be merged with next
     -> (Token b -> Bool)       -- (possibly incomplete) token should cause transition to idle state
     -> Enumeratee a (Maybe (Token b)) m t
     
tokenizeBy f pLo pHi pShort pIdle =
        EL.concatMapAccum step Idle
    =$= debounce pShort
    where
        step s@Idle x
            | pHi x     = (Pulse (f x),                 [])
            | otherwise = (s,                           [])
        
        step s@(Pulse ps) x
            | pLo x     = (Pulse (mappend ps (f x)),    [])
            | otherwise = (Delay ps (f x),              [])
        
        step s@(Delay ps ds) x
            | pHi x     = (Pulse (f x),                 [Just s])
            | pIdle s   = (Idle,                        [Just s, Nothing])
            | otherwise = (Delay ps (mappend ds (f x)), [])
            where s = Token ps ds

{-# INLINE tokenizeSamples #-}
tokenizeSamples :: (Monad m, Ord a, Fractional a) => a -> a -> Int -> Int -> Enumeratee (Samples a) (Maybe (Token (Samples a))) m b
tokenizeSamples thresholdLo thresholdHi minPulse maxDelay =
    tokenizeBy id
        ((> thresholdLo) . sampleMean)
        ((> thresholdHi) . sampleMean)
        (\t -> nSamples (pulse t) + nSamples (delay t) <  minPulse)
        ((>= maxDelay) . nSamples . delay)

{-# INLINE tokenizeCount #-}
tokenizeCount :: (Monad m, Ord a, RealFloat a) => a -> a -> Int -> Int -> Enumeratee a (Maybe (Token (Sum Int))) m b
tokenizeCount thresholdLo thresholdHi minPulse maxDelay =
    tokenizeBy (const (Sum 1))
        (> thresholdLo)
        (> thresholdHi)
        (\t -> getSum (pulse t) + getSum (delay t) <  minPulse)
        ((>= maxDelay) . getSum . delay)

{-# INLINE debounce #-}
debounce :: (Monoid a, Monad m) => (Token a -> Bool) -> Enumeratee (Maybe (Token a)) (Maybe (Token a)) m b
debounce pShort = EL.concatMapAccum accum Nothing
    where
        extend (Token p1 d1) (Token p0 d0) =
            Token (mconcat [p0, d0, p1]) d1
        
        accum Nothing       Nothing = (Nothing, [])
        accum (Just prev)   Nothing = (Nothing, [Just prev, Nothing])
        
        accum prev (Just t)
            | pShort t  = (extend t <$> prev,   [])
            | otherwise = (Just t,              [prev | isJust prev])
