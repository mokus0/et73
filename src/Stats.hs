module Stats where

import Data.Monoid

data Stats a = Stats
    { count     :: !Int
    , minVal    :: !a
    , maxVal    :: !a
    , mean      :: !a
    -- , m2        :: !a
    } deriving (Eq, Ord, Read, Show)

-- variance s = m2 s / fromIntegral (count s - 1)
-- stdev s = sqrt (variance s)

instance (Fractional a, Ord a) => Monoid (Stats a) where
    mempty = Stats
        { count     = 0
        , minVal    = 0
        , maxVal    = 0
        , mean      = 0
        -- , m2        = 0
        }
    mappend s1 s2 
        | count s1 == 0     = s2
        | count s2 == 0     = s1
        | otherwise         = Stats
            { count     = n
            , minVal    = min (minVal s1) (minVal s2)
            , maxVal    = max (maxVal s1) (maxVal s2)
            , mean      = mean s1 + delta * (fromIntegral n2 / fromIntegral n)
            -- , m2        = m2 s1 + m2 s2 + delta * delta * (fromIntegral n1 * fromIntegral n2 / fromIntegral n)
            }
            where
                n1      = count s1
                n2      = count s2
                n       = n1 + n2
                delta   = mean s2 - mean s1

stats x = Stats
        { count     = 1
        , minVal    = x
        , maxVal    = x
        , mean      = x
        -- , m2        = 0
        }
