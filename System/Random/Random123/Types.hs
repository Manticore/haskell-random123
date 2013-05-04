{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- | Type synonyms and type classes for use in function and instance declarations.
module System.Random.Random123.Types (
    Array2,
    Array4,
    LimitedInteger(..),
    Counter(..),
    Word32Array(..),
    Word64Array(..)
    ) where

import Data.Bits
import Data.Word

-- | Type synonym for a 2-element array.
type Array2 a = (a, a)

-- | Type synonym for a 4-element array.
type Array4 a = (a, a, a, a)


-- | Class of integers with more bits than in simple types yet having fixed limited size
-- (unlike the built-in 'Integer').
class LimitedInteger a where
    -- | Creates an instance from an 'Integer' (which is truncated by modulus @2^'liBitSize'@).
    liFromInteger :: Integer -> a
    -- | Creates an 'Integer' in range @[0, 2^'liBitSize')@ from an instance.
    liToInteger :: a -> Integer
    -- | Returns the size of the information in the array.
    liBitSize :: a -> Int


array2FromInteger :: Bits a => Integer -> Array2 a
array2FromInteger i = (x0, x1) where
    x1 = fromInteger i
    bits = bitSize x1 -- need this because cannot use 'a' type variable
    x0 = fromInteger (i `shiftR` bits)

array4FromInteger :: Bits a => Integer -> Array4 a
array4FromInteger i = (x0, x1, x2, x3) where
    x3 = fromInteger i
    bits = bitSize x3 -- need this because cannot use 'a' type variable
    x0 = fromInteger (i `shiftR` (bits * 3))
    x1 = fromInteger (i `shiftR` (bits * 2))
    x2 = fromInteger (i `shiftR` bits)

array2ToInteger :: (Integral a, Bits a) => Array2 a -> Integer
array2ToInteger (x0, x1) = x0' + x1' where
    bits = bitSize x0
    x0' = toInteger x0 `shiftL` bits
    x1' = toInteger x1

array4ToInteger :: (Integral a, Bits a) => Array4 a -> Integer
array4ToInteger (x0, x1, x2, x3) = x0' + x1' + x2' + x3' where
    bits = bitSize x0
    x0' = toInteger x0 `shiftL` (bits * 3)
    x1' = toInteger x1 `shiftL` (bits * 2)
    x2' = toInteger x2 `shiftL` bits
    x3' = toInteger x3

-- Technically, Word32 and Word64 instances are identical,
-- but I couldn't persuade GHC to compile them in generalized form
-- (like "instance (Num a, Bits a, Integral a) => LimitedInteger (Array2 a)").

instance LimitedInteger Word32 where
    liFromInteger = fromInteger
    liToInteger = toInteger
    liBitSize = bitSize

instance LimitedInteger (Array2 Word32) where
    liFromInteger = array2FromInteger
    liToInteger = array2ToInteger
    liBitSize _ = bitSize (undefined :: Word32) * 2

instance LimitedInteger (Array4 Word32) where
    liFromInteger = array4FromInteger
    liToInteger = array4ToInteger
    liBitSize _ = bitSize (undefined :: Word32) * 4


instance LimitedInteger Word64 where
    liFromInteger = fromInteger
    liToInteger = toInteger
    liBitSize = bitSize

instance LimitedInteger (Array2 Word64) where
    liFromInteger = array2FromInteger
    liToInteger = array2ToInteger
    liBitSize _ = bitSize (undefined :: Word64) * 2

instance LimitedInteger (Array4 Word64) where
    liFromInteger = array4FromInteger
    liToInteger = array4ToInteger
    liBitSize _ = bitSize (undefined :: Word64) * 4


-- | Class of CBRNG counters.
class LimitedInteger a => Counter a where
    -- | Skip ahead the given amount of steps.
    skip :: Integer -> a -> a
    skip i x = liFromInteger (liToInteger x + i)
    -- | Increment the counter.
    -- Usually this function is faster than @'skip' 1@.
    increment :: a -> a
    increment = skip 1


instance (LimitedInteger (Array2 a), Ord a, Num a, Bounded a) => Counter (Array2 a) where
    increment (c0, c1)
        | c1 < maxBound = (c0, c1 + 1)
        | otherwise = (c0 + 1, 0)

instance (LimitedInteger (Array4 a), Ord a, Num a, Bounded a) => Counter (Array4 a) where
    increment (c0, c1, c2, c3)
        | c3 < maxBound = (c0, c1, c2, c3 + 1)
        | c2 < maxBound = (c0, c1, c2 + 1, 0)
        | c1 < maxBound = (c0, c1 + 1, 0, 0)
        | otherwise = (c0 + 1, 0, 0, 0)


-- | Class of objects allowing the extraction of 32-bit words from the given position.
class Word32Array a where
    -- | Returns a 'Word32' from a position in range @[0, 'numWords32' - 1)@.
    getWord32 :: Int -> a -> Word32
    -- | Number of 32-bit words in this array.
    numWords32 :: a -> Int

instance Word32Array (Array2 Word32) where
    getWord32 0 (x0, _) = x0
    getWord32 1 (_, x1) = x1
    getWord32 _ _ = error "Wrong index in getWord32"
    numWords32 _ = 2

instance Word32Array (Array4 Word32) where
    getWord32 0 (x0, _, _, _) = x0
    getWord32 1 (_, x1, _, _) = x1
    getWord32 2 (_, _, x2, _) = x2
    getWord32 3 (_, _, _, x3) = x3
    getWord32 _ _ = error "Wrong index in getWord32"
    numWords32 _ = 4

instance Word32Array (Array2 Word64) where
    getWord32 0 (x0, _) = fromIntegral (x0 `shiftR` 32)
    getWord32 1 (x0, _) = fromIntegral x0
    getWord32 2 (_, x1) = fromIntegral (x1 `shiftR` 32)
    getWord32 3 (_, x1) = fromIntegral x1
    getWord32 _ _ = error "Wrong index in getWord32"
    numWords32 _ = 4

instance Word32Array (Array4 Word64) where
    getWord32 0 (x0, _, _, _) = fromIntegral (x0 `shiftR` 32)
    getWord32 1 (x0, _, _, _) = fromIntegral x0
    getWord32 2 (_, x1, _, _) = fromIntegral (x1 `shiftR` 32)
    getWord32 3 (_, x1, _, _) = fromIntegral x1
    getWord32 4 (_, _, x2, _) = fromIntegral (x2 `shiftR` 32)
    getWord32 5 (_, _, x2, _) = fromIntegral x2
    getWord32 6 (_, _, _, x3) = fromIntegral (x3 `shiftR` 32)
    getWord32 7 (_, _, _, x3) = fromIntegral x3
    getWord32 _ _ = error "Wrong index in getWord32"
    numWords32 _ = 8


-- | Class of objects allowing the extraction of 64-bit words from a given position.
class Word64Array a where
    -- | Returns a 'Word64' from a position in range @[0, 'numWords64' - 1)@.
    getWord64 :: Int -> a -> Word64
    -- | Number of 64-bit words in this array.
    numWords64 :: a -> Int


instance Word64Array (Array2 Word32) where
    getWord64 0 (x0, x1) = hi `shiftL` 32 + lo where
        lo = fromIntegral x1 :: Word64
        hi = fromIntegral x0 :: Word64
    getWord64 _ _ = error "Wrong index in getWord64"
    numWords64 _ = 1

instance Word64Array (Array4 Word32) where
    getWord64 0 (x0, x1, _, _) = hi `shiftL` 32 + lo where
        lo = fromIntegral x1 :: Word64
        hi = fromIntegral x0 :: Word64
    getWord64 1 (_, _, x2, x3) = hi `shiftL` 32 + lo where
        lo = fromIntegral x2 :: Word64
        hi = fromIntegral x3 :: Word64
    getWord64 _ _ = error "Wrong index in getWord64"
    numWords64 _ = 2

instance Word64Array (Array2 Word64) where
    getWord64 0 (x0, _) = x0
    getWord64 1 (_, x1) = x1
    getWord64 _ _ = error "Wrong index in getWord64"
    numWords64 _ = 2

instance Word64Array (Array4 Word64) where
    getWord64 0 (x0, _, _, _) = x0
    getWord64 1 (_, x1, _, _) = x1
    getWord64 2 (_, _, x2, _) = x2
    getWord64 3 (_, _, _, x3) = x3
    getWord64 _ _ = error "Wrong index in getWord64"
    numWords64 _ = 4
