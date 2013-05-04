-- | Test standard RandomGen interface of Random123-backed generators.
module TestRandomGen (test_randomgen) where

import System.Random

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import System.Random.Random123


-- Expected mean and standard deviation for a list of uniformly distributed random floats
num_floats = 10000
e_mean = 0.5
e_var = 1 / 12
e_std = sqrt e_var
e_mean_std = e_std / sqrt (fromIntegral num_floats)
e_std_std = sqrt ((e_var ** 2) / (fromIntegral num_floats - 1) * 2)

-- Functions to calculate mean and stddev of a list (not very effective)
mean xs = sum xs / fromIntegral (length xs)
std xs = sqrt (sum xsquares / fromIntegral (length xs)) where
    xmean = mean xs
    xsquares = map (\x -> (x - xmean) ** 2) xs

-- Check that mean and stddev of a generated list are inside [-5,5] sigma interval
-- around their expected values (chance of failure ~1e-6).

test_gen32_mean = (xmean < e_mean + 5 * e_std && xmean > e_mean - 5 * e_std) @?= True where
    gen = mkCBRNG32 123456
    floats = take num_floats (randoms gen :: [Float])
    xmean = mean floats

test_gen32_std = (xstd < e_std + 5 * e_std_std && xstd > e_std - 5 * e_std_std) @?= True where
    gen = mkCBRNG32 123456
    floats = take num_floats (randoms gen :: [Float])
    xstd = std floats

test_gen64_mean = (xmean < e_mean + 5 * e_std && xmean > e_mean - 5 * e_std) @?= True where
    gen = mkCBRNG64 123456
    floats = take num_floats (randoms gen :: [Float])
    xmean = mean floats

test_gen64_std = (xstd < e_std + 5 * e_std_std && xstd > e_std - 5 * e_std_std) @?= True where
    gen = mkCBRNG64 123456
    floats = take num_floats (randoms gen :: [Float])
    xstd = std floats


test_randomgen = testGroup "RandomGen" [
    testCase "Default 32-bit (mean of array of floats)" test_gen32_mean,
    testCase "Default 32-bit (std of array of floats)" test_gen32_std,
    testCase "Default 64-bit (mean of array of floats)" test_gen64_mean,
    testCase "Default 64-bit (std of array of floats)" test_gen64_std
    ]
