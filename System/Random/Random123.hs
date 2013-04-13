{-# LANGUAGE FlexibleInstances #-}

module System.Random.Random123 where

import Data.Word
import Data.Bits
import System.Random

import System.Random.Random123.Philox
import System.Random.Random123.Threefry


class Key a where
    fromGlobal :: IO a
    fromSeed :: Int -> a

instance Key Word32 where
    fromGlobal = undefined
    fromSeed = undefined


class Counter a where
    skip :: Int -> a -> a
    increment :: a -> a
    increment = skip 1
    getWord32 :: a -> a -> Int -> (Word32, a, Int)
    getWord64 :: a -> a -> Int -> (Word64, a, Int)


instance Counter (Word32, Word32) where
    skip = undefined
    increment (c0, c1)
        | c1 == 2^32-1 = (c0 + 1, 0)
        | otherwise = (c0, c1 + 1)
    getWord32 c (r0, r1) 0 = (r0, c, 1)
    getWord32 c (r0, r1) 1 = (r1, increment c, 0)
    getWord32 c (r0, r1) _ = error "Incorrect word counter state for getWord32"
    getWord64 c (r0, r1) 0 = (shiftL hi 32 + lo, increment c, 0) where
        lo = fromIntegral r1 :: Word64
        hi = fromIntegral r0 :: Word64
    getWord64 c (r0, r1) _ = error "Incorrect word counter state for getWord64"


data StatefulCBRNG k c = StatefulCBRNG (k -> c -> c) k c Int


instance (Show k, Show c) => Show (StatefulCBRNG k c) where
    show (StatefulCBRNG biject key ctr w32_ctr) =
        unwords ["StatefulCBRNG", show key, show ctr, show w32_ctr]


instance (Key k, Counter c) => RandomGen (StatefulCBRNG k c) where

    next (StatefulCBRNG biject key ctr w32_ctr) = (int_num, new_gen) where
        full_random = biject key ctr
        (int_num, new_ctr, new_w32_ctr) = if (maxBound :: Int) <= 2^31-1
            then getInt32 (getWord32 ctr full_random w32_ctr)
            else getInt64 (getWord64 ctr full_random w32_ctr)
        getInt32 :: (Word32, k, c) -> (Int, k, c)
        getInt32 (raw_num, c, wc) = ((fromIntegral raw_num) :: Int, c, wc)
        getInt64 :: (Word64, k, c) -> (Int, k, c)
        getInt64 (raw_num, c, wc) = ((fromIntegral raw_num) :: Int, c, wc)
        new_gen = StatefulCBRNG biject key new_ctr new_w32_ctr

    genRange _
        | maxb <= 2^63-1 = (minb, maxb)
        | otherwise = (-2^63, 2^63-1)
        where
            minb = minBound :: Int
            maxb = maxBound :: Int

    split gen = (gen, gen)


main = do
    let
        key = 0 :: Word32
        ctr = (2345 :: Word32, 124 :: Word32)
        cbrng = StatefulCBRNG philox2 key ctr 0
        in do
            print $ next cbrng
            print $ next cbrng
            print $ next cbrng
            print $ next cbrng
            print $ next cbrng
            print $ genRange cbrng

