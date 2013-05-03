-- | Test standard RandomGen interface of Random123-backed generators.
module TestRandomGen (test_randomgen) where

import Data.Word
import System.Random

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import System.Random.Random123.Types
import System.Random.Random123.RandomGen


num_floats = 10000
e_mean = 0.5
e_var = 1 / 12
e_std = sqrt e_var
e_mean_std = e_std / (sqrt (fromIntegral num_floats))
e_std_std = sqrt ((e_var ^^ 2) / (fromIntegral num_floats - 1) * 2)

test_gen32_mean = (avg < e_mean + 5 * e_std && avg > e_mean - 5 * e_std) @?= True where
    gen = mkCBRNG32 123456
    rlist = take num_floats (randoms gen :: [Float])
    avg = (sum rlist) / (fromIntegral num_floats)

test_gen32_std = (std < e_std + 5 * e_std_std && std > e_std - 5 * e_std_std) @?= True where
    gen = mkCBRNG32 123456
    rlist = take num_floats (randoms gen :: [Float])
    avg = (sum rlist) / (fromIntegral num_floats)
    std = sqrt ((sum (map (\x -> (x - avg) ^^ 2) rlist)) / (fromIntegral num_floats))

test_gen64_mean = (avg < e_mean + 5 * e_std && avg > e_mean - 5 * e_std) @?= True where
    gen = mkCBRNG64 123456
    rlist = take num_floats (randoms gen :: [Float])
    avg = (sum rlist) / (fromIntegral num_floats)

test_gen64_std = (std < e_std + 5 * e_std_std && std > e_std - 5 * e_std_std) @?= True where
    gen = mkCBRNG64 123456
    rlist = take num_floats (randoms gen :: [Float])
    avg = (sum rlist) / (fromIntegral num_floats)
    std = sqrt ((sum (map (\x -> (x - avg) ^^ 2) rlist)) / (fromIntegral num_floats))


test_randomgen = testGroup "RandomGen" [
    testCase "Default 32-bit (mean of array of floats)" test_gen32_mean,
    testCase "Default 32-bit (std of array of floats)" test_gen32_std,
    testCase "Default 64-bit (mean of array of floats)" test_gen64_mean,
    testCase "Default 64-bit (std of array of floats)" test_gen64_std
    ]
