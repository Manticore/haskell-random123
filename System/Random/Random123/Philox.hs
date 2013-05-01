{-# OPTIONS_HADDOCK not-home #-}

module System.Random.Random123.Philox (
    philox2, philox4, philox2R, philox4R, PhiloxWord) where

import Data.Word
import Data.Bits

import System.Random.Random123.Misc
import System.Random.Random123.Types


-- | Class of integer types suitable for use in Philox algorithm.
class (Bits a, Num a) => PhiloxWord a where
    mulhilo :: a -> a -> Array2 a
    philoxW_0 :: a
    philoxW_1 :: a
    philoxM2 :: a
    philoxM4_0 :: a
    philoxM4_1 :: a


instance PhiloxWord Word32 where
    mulhilo a b = (hi, lo) where
        r :: Word64
        r = fromIntegral a * fromIntegral b
        hi = fromIntegral (r `shiftR` 32)
        lo = fromIntegral r

    philoxW_0 = 0x9E3779B9 -- golden ratio
    philoxW_1 = 0xBB67AE85 -- sqrt(3)-1

    philoxM2 = 0xD256D193

    -- The order is swapped as compared to the reference implementation,
    -- to make the call order in philoxRound4 more logical.
    philoxM4_0 = 0xCD9E8D57
    philoxM4_1 = 0xD2511F53


instance PhiloxWord Word64 where
    mulhilo a b = (hi, lo) where
        r :: Integer
        r = fromIntegral a * fromIntegral b
        hi = fromIntegral (r `shiftR` 64)
        lo = fromIntegral r

    philoxW_0 = 0x9E3779B97F4A7C15 -- golden ratio
    philoxW_1 = 0xBB67AE8584CAA73B -- sqrt(3)-1

    philoxM2 = 0xD2B74407B1CE6E93

    -- The order is swapped as compared to the reference implementation,
    -- to make the call order in philoxRound4 more logical.
    philoxM4_0 = 0xCA5A826395121157
    philoxM4_1 = 0xD2E7470EE14C6C93


philoxSubround :: PhiloxWord a => Int -> a -> a -> a -> Array2 a -> Array2 a
philoxSubround r w m k (x0, x1) = (x0', x1') where
    k' = k + w * (fromIntegral r)
    (hi, lo) = mulhilo m x0
    x0' = hi `xor` k' `xor` x1
    x1' = lo

philoxRound2 :: PhiloxWord a => a -> Int -> Array2 a -> Array2 a
philoxRound2 k r (x0, x1) = (x0', x1') where
    (x0', x1') = philoxSubround r philoxW_0 philoxM2 k (x0, x1)

philoxRound4 :: PhiloxWord a => Array2 a -> Int -> Array4 a -> Array4 a
philoxRound4 (k0, k1) r (x0, x1, x2, x3) = (x0', x1', x2', x3') where
    (x0', x1') = philoxSubround r philoxW_0 philoxM4_0 k0 (x2, x1)
    (x2', x3') = philoxSubround r philoxW_1 philoxM4_1 k1 (x0, x3)


-- | Generates a Philox-2 random number with a custom number of rounds.
philox2R :: PhiloxWord a
    => Int -- ^ number of rounds (1-16),
    -> a -- ^ key,
    -> Array2 a -- ^ counter,
    -> Array2 a -- ^ random number.
philox2R rounds key ctr
    | (rounds >= 1) && (rounds <= 16) = apply (philoxRound2 key) rounds ctr
    | otherwise = error "The number of rounds in Philox-2 must be between 1 and 16"


-- | Generates a Philox-4 random number with a custom number of rounds.
philox4R :: PhiloxWord a
    => Int -- ^ number of rounds (1-16),
    -> Array2 a -- ^ key,
    -> Array4 a -- ^ counter,
    -> Array4 a -- ^ random number.
philox4R rounds key ctr
    | (rounds >= 1) && (rounds <= 16) = apply (philoxRound4 key) rounds ctr
    | otherwise = error "The number of rounds in Philox-4 must be between 1 and 16"


-- | Generates a Philox-2 random number with the optimal number of rounds.
philox2 :: PhiloxWord a
    => a -- ^ key,
    -> Array2 a -- ^ counter,
    -> Array2 a -- ^ random number.
philox2 = philox2R 10


-- | Generates a Philox-4 random number with the optimal number of rounds.
philox4 :: PhiloxWord a
    => Array2 a -- ^ key,
    -> Array4 a -- ^ counter,
    -> Array4 a -- ^ random number.
philox4 = philox4R 10
