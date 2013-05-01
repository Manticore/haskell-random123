{-# OPTIONS_HADDOCK not-home #-}

-- | Threefry, a counter-based random number generator.
module System.Random.Random123.Threefry (
    threefry2, threefry4, threefry2R, threefry4R, ThreefryWord) where

import Data.Word
import Data.Bits
import Data.Array.Base
import Data.Array.Unboxed


import System.Random.Random123.Types
import System.Random.Random123.Misc


-- Helper type to store small constant arrays of rotation constants,
-- and accompanying function for their extraction.
-- Not sure if UArray and unsafeAt are the best choice.

type RotationConstants = UArray Int Int

getRotationConstant :: RotationConstants -> Int -> Int
getRotationConstant = unsafeAt


-- | Class of integer types suitable for use in Threefry algorithm.
class ThreefryWord a where
    parityConstant :: a
    rotationConstant2 :: a -> RotationConstants
    rotationConstant4_0 :: a -> RotationConstants
    rotationConstant4_1 :: a -> RotationConstants


instance ThreefryWord Word32 where
    parityConstant = 0x1BD11BDA

    -- Output from skein_rot_search (srs32x2-X5000.out)
    -- Random seed = 1. BlockSize = 64 bits. sampleCnt =  1024. rounds =  8, minHW_or=28
    -- Start: Tue Jul 12 11:11:33 2011
    -- rMin = 0.334. #0206[*07] [CRC=1D9765C0. hw_OR=32. cnt=16384. blkSize=  64].format
    -- 4 rounds: minHW =  4  [  4  4  4  4 ]
    -- 5 rounds: minHW =  6  [  6  8  6  8 ]
    -- 6 rounds: minHW =  9  [  9 12  9 12 ]
    -- 7 rounds: minHW = 16  [ 16 24 16 24 ]
    -- 8 rounds: minHW = 32  [ 32 32 32 32 ]
    -- 9 rounds: minHW = 32  [ 32 32 32 32 ]
    -- 10 rounds: minHW = 32  [ 32 32 32 32 ]
    -- 11 rounds: minHW = 32  [ 32 32 32 32 ]
    rotationConstant2 _ = listArray (0, 7) [13, 15, 26, 6, 17, 29, 16, 24]

    -- Output from skein_rot_search: (srs-B128-X5000.out)
    -- Random seed = 1. BlockSize = 64 bits. sampleCnt =  1024. rounds =  8, minHW_or=28
    -- Start: Mon Aug 24 22:41:36 2009
    -- ...
    -- rMin = 0.472. #0A4B[*33] [CRC=DD1ECE0F. hw_OR=31. cnt=16384. blkSize= 128].format
    -- 4 rounds: minHW =  3  [  3  3  3  3 ]
    -- 5 rounds: minHW =  7  [  7  7  7  7 ]
    -- 6 rounds: minHW = 12  [ 13 12 13 12 ]
    -- 7 rounds: minHW = 22  [ 22 23 22 23 ]
    -- 8 rounds: minHW = 31  [ 31 31 31 31 ]
    -- 9 rounds: minHW = 32  [ 32 32 32 32 ]
    -- 10 rounds: minHW = 32  [ 32 32 32 32 ]
    -- 11 rounds: minHW = 32  [ 32 32 32 32 ]
    rotationConstant4_0 _ = listArray (0, 7) [10, 11, 13, 23, 6, 17, 25, 18]
    rotationConstant4_1 _ = listArray (0, 7) [26, 21, 27, 5, 20, 11, 10, 20]


instance ThreefryWord Word64 where
    parityConstant = 0x1BD11BDAA9FC1A22

    -- Output from skein_rot_search: (srs64_B64-X1000)
    -- Random seed = 1. BlockSize = 128 bits. sampleCnt =  1024. rounds =  8, minHW_or=57
    -- Start: Tue Mar  1 10:07:48 2011
    -- rMin = 0.136. #0325[*15] [CRC=455A682F. hw_OR=64. cnt=16384. blkSize= 128].format
    -- 4 rounds: minHW =  4  [  4  4  4  4 ]
    -- 5 rounds: minHW =  8  [  8  8  8  8 ]
    -- 6 rounds: minHW = 16  [ 16 16 16 16 ]
    -- 7 rounds: minHW = 32  [ 32 32 32 32 ]
    -- 8 rounds: minHW = 64  [ 64 64 64 64 ]
    -- 9 rounds: minHW = 64  [ 64 64 64 64 ]
    -- 10 rounds: minHW = 64  [ 64 64 64 64 ]
    -- 11 rounds: minHW = 64  [ 64 64 64 64 ]
    rotationConstant2 _ = listArray (0, 7) [16, 42, 12, 31, 16, 32, 24, 21]

    -- These are the R_256 constants from the Threefish reference sources
    -- with names changed to R_64x4...
    rotationConstant4_0 _ = listArray (0, 7) [14, 52, 23, 5, 25, 46, 58, 32]
    rotationConstant4_1 _ = listArray (0, 7) [16, 57, 40, 37, 33, 12, 22, 32]


-- S-box

sbox' :: Bits a => Int -> (a -> RotationConstants) -> Array2 a -> Array2 a
sbox' r r_constant (x0, x1) = (x0', x1') where
    rot = getRotationConstant (r_constant (undefined :: a)) (r `mod` 8)
    x0' = x0 + x1
    x1' = x0' `xor` (x1 `rotate` rot)

sbox2 :: (ThreefryWord a, Bits a) => Int -> Array2 a -> Array2 a
sbox2 r x = sbox' r rotationConstant2 x

sbox4 :: (ThreefryWord a, Bits a) => Int -> Array4 a -> Array4 a
sbox4 r (x0, x1, x2, x3) = (x0', x1', x2', x3') where
    (xa, xb) = if r `mod` 2 == 0 then (x1, x3) else (x3, x1)
    (x0', xa') = sbox' r rotationConstant4_0 (x0, xa)
    (x2', xb') = sbox' r rotationConstant4_1 (x2, xb)
    (x1', x3') = if r `mod` 2 == 0 then (xa', xb') else (xb', xa')


-- P-box

shiftTuple2 :: Int -> (a, a, a) -> Array2 a
shiftTuple2 i (k0, k1, k2)
    | remainder == 0 = (k0, k1)
    | remainder == 1 = (k1, k2)
    | otherwise = (k2, k0)
    where
        remainder = i `mod` 3

shiftTuple4 :: Int -> (a, a, a, a, a) -> Array4 a
shiftTuple4 i (k0, k1, k2, k3, k4)
    | remainder == 0 = (k0, k1, k2, k3)
    | remainder == 1 = (k1, k2, k3, k4)
    | remainder == 2 = (k2, k3, k4, k0)
    | remainder == 3 = (k3, k4, k0, k1)
    | otherwise = (k4, k0, k1, k2)
    where
        remainder = i `mod` 5

addTuple2 :: Num a => Array2 a -> Array2 a -> Array2 a
addTuple2 (k0, k1) (x0, x1) = (k0 + x0, k1 + x1)

addTuple4 :: Num a => Array4 a -> Array4 a -> Array4 a
addTuple4 (k0, k1, k2, k3) (x0, x1, x2, x3) = (k0 + x0, k1 + x1, k2 + x2, k3 + x3)

pbox2 :: Bits a => (a, a, a) -> Int -> Array2 a -> Array2 a
pbox2 extended_key r x = (x0', x1' + fromIntegral shift) where
    shift = r `div` 4 + 1
    (x0', x1') = addTuple2 x (shiftTuple2 shift extended_key)

pbox4 :: Bits a => (a, a, a, a, a) -> Int -> Array4 a -> Array4 a
pbox4 extended_key r x = (x0', x1', x2', x3' + fromIntegral shift) where
    shift = r `div` 4 + 1
    (x0', x1', x2', x3') = addTuple4 x (shiftTuple4 shift extended_key)


-- Additional helper functions.

threefryRound pbox sbox r x = if r `mod` 4 == 3
    then pbox r (sbox r x)
    else sbox r x

extendKey2 :: (ThreefryWord a, Bits a) => Array2 a -> (a, a, a)
extendKey2 (k0, k1) = (k0, k1, k0 `xor` k1 `xor` parityConstant)

extendKey4 :: (ThreefryWord a, Bits a) => Array4 a -> (a, a, a, a, a)
extendKey4 (k0, k1, k2, k3) = (k0, k1, k2, k3, k0 `xor` k1 `xor` k2 `xor` k3 `xor` parityConstant)


-- | Generates a Threefry-2 random number with a custom number of rounds.
threefry2R :: (ThreefryWord a, Bits a)
    => Int -- ^ number of rounds (1-32),
    -> Array2 a -- ^ key,
    -> Array2 a -- ^ counter,
    -> Array2 a -- ^ random number.
threefry2R rounds key ctr
    | (rounds >= 1) && (rounds <= 32) = apply (threefryRound pbox sbox2) rounds starting_x
    | otherwise = error "The number of rounds in Threefry-2 must be between 1 and 32"
    where
        starting_x = addTuple2 key ctr
        pbox = pbox2 (extendKey2 key)


-- | Generates a Threefry-4 random number with a custom number of rounds.
threefry4R :: (ThreefryWord a, Bits a)
    => Int -- ^ number of rounds (1-72),
    -> Array4 a -- ^ key,
    -> Array4 a -- ^ counter,
    -> Array4 a -- ^ random number.
threefry4R rounds key ctr
    | (rounds >= 1) && (rounds <= 72) = apply (threefryRound pbox sbox4) rounds starting_x
    | otherwise = error "The number of rounds in Threefry-4 must be between 1 and 72"
    where
        starting_x = addTuple4 key ctr
        pbox = pbox4 (extendKey4 key)


-- | Generates a Threefry-2 random number with the optimal number of rounds.
threefry2 :: (ThreefryWord a, Bits a)
    => Array2 a -- ^ key,
    -> Array2 a -- ^ counter,
    -> Array2 a -- ^ random number.
threefry2 = threefry2R 20


-- | Generates a Threefry-4 random number with the optimal number of rounds.
threefry4 :: (ThreefryWord a, Bits a)
    => Array4 a -- ^ key,
    -> Array4 a -- ^ counter,
    -> Array4 a -- ^ random number.
threefry4 = threefry4R 20
