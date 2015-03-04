module Samples
    ( Samples
    , sample
    , addSample
    
    , nSamples
    , sampleMean
    , duration
    , sampleFreq
    )where

import Data.Monoid

updateMean n m x = m + (x - m) / fromIntegral n
mergeMean alpha m0 m1 = m0 + (m1 - m0) * alpha

data Samples a = Samples
    { nSamples      :: !Int
    , sampleMean    :: !a
    , freqMean      :: !a
    }
    deriving (Eq, Ord, Read, Show)

instance (Ord a, Fractional a) => Monoid (Samples a) where
    {-# INLINE mempty #-}
    mempty  = Samples 0 0 0
    
    {-# INLINE mappend #-}
    mappend (Samples n0 x0 p0) (Samples n1 x1 p1) = 
        Samples n (mergeMean alpha x0 x1) (mergeMean alpha p0 p1)
            where
                n = n0 + n1
                alpha = fromIntegral n1 / fromIntegral n

{-# INLINE sample #-}
sample       x p = Samples 1 x p

{-# INLINE addSample #-}
addSample (Samples n0 x0 p0) x1 p1 = Samples n1 (updateMean n1 x0 x1) (updateMean n1 p0 p1)
    where
        n1 = n0 + 1

duration   rate t = fromIntegral (nSamples t) / rate
sampleFreq rate t = rate * freqMean t

