module System.Random.Random123.Philox (
    philox2, philox4, philox2R, philox4R) where

import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Array.Base

class (Bits a, Num a) => PhiloxWord a where
    mulhilo :: a -> a -> (a, a)
    philoxW :: Int -> a
    philoxM2 :: a
    philoxM4 :: Int -> a

instance PhiloxWord Word32 where
    mulhilo a b = (hi, lo) where
        r :: Word64
        r = fromIntegral a * fromIntegral b
        hi = fromIntegral (r `shiftR` 32)
        lo = fromIntegral r

    philoxW 0 = 0x9E3779B9 -- golden ratio
    philoxW 1 = 0xBB67AE85 -- sqrt(3)-1

    philoxM2 = 0xD256D193
    philoxM4 0 = 0xD2511F53
    philoxM4 1 = 0xCD9E8D57

instance PhiloxWord Word64 where
    mulhilo a b = (hi, lo) where
        r :: Integer
        r = fromIntegral a * fromIntegral b
        hi = fromIntegral (r `shiftR` 64)
        lo = fromIntegral r

    philoxW 0 = 0x9E3779B97F4A7C15 -- golden ratio
    philoxW 1 = 0xBB67AE8584CAA73B -- sqrt(3)-1

    philoxM2 = 0xD2B74407B1CE6E93
    philoxM4 0 = 0xD2E7470EE14C6C93
    philoxM4 1 = 0xCA5A826395121157


philoxSubround :: PhiloxWord a => Int -> a -> a -> a -> (a, a) -> (a, a)
philoxSubround r w m k (x0, x1) = (x0', x1') where
    k' = k + w * (fromIntegral r)
    (hi, lo) = mulhilo m x0
    x0' = hi `xor` k' `xor` x1
    x1' = lo

philoxRound2 :: PhiloxWord a => a -> Int -> (a, a) -> (a, a)
philoxRound2 k r (x0, x1) = (x0', x1') where
    (x0', x1') = philoxSubround r (philoxW 0) philoxM2 k (x0, x1)

philoxRound4 :: PhiloxWord a => (a, a) -> Int -> (a, a, a, a) -> (a, a, a, a)
philoxRound4 (k0, k1) r (x0, x1, x2, x3) = (x0', x1', x2', x3') where
    (x0', x1') = philoxSubround r (philoxW 0) (philoxM4 1) k0 (x2, x1)
    (x2', x3') = philoxSubround r (philoxW 1) (philoxM4 0) k1 (x0, x3)


apply :: Int -> (Int -> a -> a) -> a -> a
apply n f v0 = applyLoop 0 v0 where
    applyLoop i v
        | i == n    = v
        | otherwise = applyLoop (i + 1) $ f i v


philox2R :: PhiloxWord a => Int -> a -> (a, a) -> (a, a)
philox2R rounds key ctr = apply rounds (philoxRound2 key) ctr

philox4R :: PhiloxWord a => Int -> (a, a) -> (a, a, a, a) -> (a, a, a, a)
philox4R rounds key ctr = apply rounds (philoxRound4 key) ctr

philox2 :: PhiloxWord a => a -> (a, a) -> (a, a)
philox2 = philox2R 10

philox4 :: PhiloxWord a => (a, a) -> (a, a, a, a) -> (a, a, a, a)
philox4 = philox4R 10
