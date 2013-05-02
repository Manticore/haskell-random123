module System.Random.Random123.RandomGen (
    mkPhilox2x32Gen,
    StatefulCBRNG32,
    StatefulCBRNG64
    ) where

import System.Random
import Data.Word

import System.Random.Random123.Types
import System.Random.Random123.Philox
import System.Random.Random123.Threefry


data StatefulCBRNG32 k c = StatefulCBRNG32 (k -> c -> c) k c Int
data StatefulCBRNG64 k c = StatefulCBRNG64 (k -> c -> c) k c Int


instance (Counter c, Word32Array c) => RandomGen (StatefulCBRNG32 k c) where

    next (StatefulCBRNG32 biject key ctr wctr) = (fromIntegral w32, new_gen) where
        arr = biject key ctr
        w32 = getWord32 wctr arr
        (ctr', wctr') = if wctr + 1 < numWords32 arr
            then (ctr, wctr + 1)
            else (increment ctr, 0)
        new_gen = StatefulCBRNG32 biject key ctr' wctr'

    genRange _ = (0, min (maxBound :: Int) (2^32 - 1))
    split gen = undefined


instance (Counter c, Word64Array c) => RandomGen (StatefulCBRNG64 k c) where

    next (StatefulCBRNG64 biject key ctr wctr) = (fromIntegral w64, new_gen) where
        arr = biject key ctr
        w64 = getWord64 wctr arr
        (ctr', wctr') = if wctr + 1 < numWords64 arr
            then (ctr, wctr + 1)
            else (increment ctr, 0)
        new_gen = StatefulCBRNG64 biject key ctr' wctr'

    genRange _ = (0, min (maxBound :: Int) (2^64 - 1))
    split gen = undefined


-- | Maps an 'Integer' into a Philox2x32 generator.
mkPhilox2x32Gen :: Integer -> StatefulCBRNG32 Word32 (Array2 Word32)
mkPhilox2x32Gen seed = StatefulCBRNG32 philox2 (liFromInteger seed) (liFromInteger 0) 0
