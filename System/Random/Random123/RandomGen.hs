module System.Random.Random123.RandomGen (
    mkCustomCBRNG32,
    mkCustomCBRNG64,
    mkCBRNG32,
    mkCBRNG64,
    CBRNG32,
    CBRNG64
    ) where

import System.Random
import Data.Word

import System.Random.Random123.Types
import System.Random.Random123.Philox
import System.Random.Random123.Threefry


data CustomCBRNG32 k c = CustomCBRNG32 (k -> c -> c) k c Int
data CustomCBRNG64 k c = CustomCBRNG64 (k -> c -> c) k c Int

data CBRNG32 = CBRNG32 (Array2 Word32) (Array4 Word32) Int deriving (Eq, Show, Read)
data CBRNG64 = CBRNG64 (Array2 Word64) (Array4 Word64) Int deriving (Eq, Show, Read)


next32 :: (Counter c, Word32Array c) => (c -> c) -> c -> Int -> (Int, c, Int)
next32 bijection ctr wctr = (fromIntegral w32, ctr', wctr') where
    arr = bijection ctr
    w32 = getWord32 wctr arr
    (ctr', wctr') = if wctr + 1 < numWords32 arr
        then (ctr, wctr + 1)
        else (increment ctr, 0)

genRange32 = (0, min maxBound (2^32 - 1)) :: (Int, Int)

next64 :: (Counter c, Word64Array c) => (c -> c) -> c -> Int -> (Int, c, Int)
next64 bijection ctr wctr = (fromIntegral w64, ctr', wctr') where
    arr = bijection ctr
    w64 = getWord64 wctr arr
    (ctr', wctr') = if wctr + 1 < numWords64 arr
        then (ctr, wctr + 1)
        else (increment ctr, 0)

genRange64 = (0, min maxBound (2^64 - 1)) :: (Int, Int)


instance (Counter c, Word32Array c) => RandomGen (CustomCBRNG32 k c) where
    next (CustomCBRNG32 bijection key ctr wctr) = (res, new_gen) where
        (res, ctr', wctr') = next32 (bijection key) ctr wctr
        new_gen = CustomCBRNG32 bijection key ctr' wctr'
    genRange _ = genRange32
    split gen = undefined

instance (Counter c, Word64Array c) => RandomGen (CustomCBRNG64 k c) where
    next (CustomCBRNG64 bijection key ctr wctr) = (res, new_gen) where
        (res, ctr', wctr') = next64 (bijection key) ctr wctr
        new_gen = CustomCBRNG64 bijection key ctr' wctr'
    genRange _ = genRange64
    split gen = undefined

instance RandomGen CBRNG32 where
    next (CBRNG32 key ctr wctr) = (res, new_gen) where
        (res, ctr', wctr') = next32 (philox4 key) ctr wctr
        new_gen = CBRNG32 key ctr' wctr'
    genRange _ = genRange32
    split gen = undefined

instance RandomGen CBRNG64 where
    next (CBRNG64 key ctr wctr) = (res, new_gen) where
        (res, ctr', wctr') = next64 (philox4 key) ctr wctr
        new_gen = CBRNG64 key ctr' wctr'
    genRange _ = genRange64
    split gen = undefined


-- | Creates a custom 32-bit RNG from a keyed bijection
-- ('Word32'- or 'Word64'-parametrized version of 'philox2', 'philox2R', 'philox4', 'philox4R',
-- 'threefry2', 'threefry2R', 'threefry4', 'threefry4R')
-- and a corresponding key.
mkCustomCBRNG32 :: LimitedInteger c => (k -> c -> c) -> k -> CustomCBRNG32 k c
mkCustomCBRNG32 bijection key = CustomCBRNG32 bijection key (liFromInteger 0) 0

-- | Creates a custom 64-bit RNG from a keyed bijection
-- ('Word32'- or 'Word64'-parametrized version of 'philox2', 'philox2R', 'philox4', 'philox4R',
-- 'threefry2', 'threefry2R', 'threefry4', 'threefry4R')
-- and a corresponding key.
mkCustomCBRNG64 :: LimitedInteger c => (k -> c -> c) -> k -> CustomCBRNG64 k c
mkCustomCBRNG64 bijection key = CustomCBRNG64 bijection key (liFromInteger 0) 0

-- | Creates a default 32-bit RNG (based on 32-bit 'philox4') with an 'Integer' key.
mkCBRNG32 :: Integer -> CBRNG32
mkCBRNG32 key = CBRNG32 (liFromInteger key) (liFromInteger 0) 0

-- | Creates a default 64-bit RNG (based on 64-bit 'philox4') with an 'Integer' key.
mkCBRNG64 :: Integer -> CBRNG64
mkCBRNG64 key = CBRNG64 (liFromInteger key) (liFromInteger 0) 0
