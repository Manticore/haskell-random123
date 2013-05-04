{-# OPTIONS_HADDOCK not-home #-}

-- | Integration with the standard library 'RandomGen' class.
module System.Random.Random123.RandomGen (
    mkCustomCBRNG32,
    mkCustomCBRNG64,
    restoreCustomCBRNG32,
    restoreCustomCBRNG64,
    mkCBRNG32,
    mkCBRNG64,
    restoreCBRNG32,
    restoreCBRNG64,
    CBRNG32,
    CBRNG64,
    CustomCBRNG32,
    CustomCBRNG64,
    CBRNGState,
    SerializableCBRNG(..)
    ) where

import System.Random
import Data.Word

import System.Random.Random123.Types
import System.Random.Random123.Philox
import System.Random.Random123.Threefry


-- | 32-bit RNG with a custom bijection function.
-- Can be serialized with 'getState' and restored with 'restoreCustomCBRNG32'
-- (but it is the user's responsibility to provide the original bijection).
data CustomCBRNG32 k c = CustomCBRNG32 (k -> c -> c) k c Int

-- | 64-bit RNG with a custom bijection function.
-- Can be serialized with 'getState' and restored with 'restoreCustomCBRNG32'
-- (but it is the user's responsibility to provide the original bijection).
data CustomCBRNG64 k c = CustomCBRNG64 (k -> c -> c) k c Int

-- | Default 32-bit RNG.
-- Supports serialization through 'Show' / 'Read' interface.
-- Alternatively, can be serialized with 'getState' and restored with 'restoreCBRNG32'.
data CBRNG32 = CBRNG32 (Array2 Word32) (Array4 Word32) Int deriving (Eq, Show, Read)

-- | Default 64-bit RNG.
-- Supports serialization through 'Show' / 'Read' interface.
-- Alternatively, can be serialized with 'getState' and restored with 'restoreCBRNG64'.
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
    split (CustomCBRNG32 bijection key ctr wctr) = (gen', gen'') where
        ctr' = increment ctr
        ctr'' = bijection key ctr'
        gen' = CustomCBRNG32 bijection key ctr' 0
        gen'' = CustomCBRNG32 bijection key ctr'' 0

instance (Counter c, Word64Array c) => RandomGen (CustomCBRNG64 k c) where
    next (CustomCBRNG64 bijection key ctr wctr) = (res, new_gen) where
        (res, ctr', wctr') = next64 (bijection key) ctr wctr
        new_gen = CustomCBRNG64 bijection key ctr' wctr'
    genRange _ = genRange64
    split (CustomCBRNG64 bijection key ctr wctr) = (gen', gen'') where
        ctr' = increment ctr
        ctr'' = bijection key ctr'
        gen' = CustomCBRNG64 bijection key ctr' 0
        gen'' = CustomCBRNG64 bijection key ctr'' 0

instance RandomGen CBRNG32 where
    next (CBRNG32 key ctr wctr) = (res, new_gen) where
        (res, ctr', wctr') = next32 (philox4 key) ctr wctr
        new_gen = CBRNG32 key ctr' wctr'
    genRange _ = genRange32
    split (CBRNG32 key ctr wctr) = (gen', gen'') where
        ctr' = increment ctr
        ctr'' = philox4 key ctr'
        gen' = CBRNG32 key ctr' 0
        gen'' = CBRNG32 key ctr'' 0

instance RandomGen CBRNG64 where
    next (CBRNG64 key ctr wctr) = (res, new_gen) where
        (res, ctr', wctr') = next64 (philox4 key) ctr wctr
        new_gen = CBRNG64 key ctr' wctr'
    genRange _ = genRange64
    split (CBRNG64 key ctr wctr) = (gen', gen'') where
        ctr' = increment ctr
        ctr'' = philox4 key ctr'
        gen' = CBRNG64 key ctr' 0
        gen'' = CBRNG64 key ctr'' 0

-- | Generalized CBRNG state, consisting of key, counter and subcounter,
-- where the first two are cast to integers (using 'liToInteger').
data CBRNGState = CBRNGState Integer Integer Int deriving (Eq, Show, Read)


-- | Class of RNGs allowing the state extraction.
class SerializableCBRNG a where
    getState :: a -> CBRNGState


instance SerializableCBRNG CBRNG32 where
    getState (CBRNG32 key ctr wctr) = CBRNGState (liToInteger key) (liToInteger ctr) wctr

instance SerializableCBRNG CBRNG64 where
    getState (CBRNG64 key ctr wctr) = CBRNGState (liToInteger key) (liToInteger ctr) wctr

instance (LimitedInteger k, LimitedInteger c) => SerializableCBRNG (CustomCBRNG32 k c) where
    getState (CustomCBRNG32 bijection key ctr wctr) =
        CBRNGState (liToInteger key) (liToInteger ctr) wctr

instance (LimitedInteger k, LimitedInteger c) => SerializableCBRNG (CustomCBRNG64 k c) where
    getState (CustomCBRNG64 bijection key ctr wctr) =
        CBRNGState (liToInteger key) (liToInteger ctr) wctr


-- | Creates a custom 32-bit RNG from a keyed bijection
-- ('Word32'- or 'Word64'-parametrized version of 'philox2', 'philox2R', 'philox4', 'philox4R',
-- 'threefry2', 'threefry2R', 'threefry4', 'threefry4R')
-- and a corresponding key.
mkCustomCBRNG32 :: LimitedInteger c => (k -> c -> c) -> k -> CustomCBRNG32 k c
mkCustomCBRNG32 bijection key = CustomCBRNG32 bijection key (liFromInteger 0) 0

-- | Restores a custom 32-bit RNG from a saved state.
restoreCustomCBRNG32 :: (LimitedInteger k, LimitedInteger c)
    => (k -> c -> c) -> CBRNGState -> CustomCBRNG32 k c
restoreCustomCBRNG32 bijection (CBRNGState key ctr wctr) =
    CustomCBRNG32 bijection (liFromInteger key) (liFromInteger ctr) wctr

-- | Creates a custom 64-bit RNG from a keyed bijection
-- ('Word32'- or 'Word64'-parametrized version of 'philox2', 'philox2R', 'philox4', 'philox4R',
-- 'threefry2', 'threefry2R', 'threefry4', 'threefry4R')
-- and a corresponding key.
mkCustomCBRNG64 :: LimitedInteger c => (k -> c -> c) -> k -> CustomCBRNG64 k c
mkCustomCBRNG64 bijection key = CustomCBRNG64 bijection key (liFromInteger 0) 0

-- | Restores a custom 64-bit RNG from a saved state.
restoreCustomCBRNG64 :: (LimitedInteger k, LimitedInteger c)
    => (k -> c -> c) -> CBRNGState -> CustomCBRNG64 k c
restoreCustomCBRNG64 bijection (CBRNGState key ctr wctr) =
    CustomCBRNG64 bijection (liFromInteger key) (liFromInteger ctr) wctr

-- | Creates a default 32-bit RNG (based on 32-bit 'philox4') with an 'Integer' key.
mkCBRNG32 :: Integer -> CBRNG32
mkCBRNG32 key = CBRNG32 (liFromInteger key) (liFromInteger 0) 0

-- | Restores a default 32-bit RNG from a saved state.
restoreCBRNG32 :: CBRNGState -> CBRNG32
restoreCBRNG32 (CBRNGState key ctr wctr) = CBRNG32 (liFromInteger key) (liFromInteger ctr) wctr

-- | Creates a default 64-bit RNG (based on 64-bit 'philox4') with an 'Integer' key.
mkCBRNG64 :: Integer -> CBRNG64
mkCBRNG64 key = CBRNG64 (liFromInteger key) (liFromInteger 0) 0

-- | Restores a default 64-bit RNG from a saved state.
restoreCBRNG64 :: CBRNGState -> CBRNG64
restoreCBRNG64 (CBRNGState key ctr wctr) = CBRNG64 (liFromInteger key) (liFromInteger ctr) wctr
