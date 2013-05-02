-- | Test standard RandomGen interface of Random123-backed generators.
module TestRandomGen (test_randomgen) where

import Data.Word
import System.Random

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import System.Random.Random123.Types
import System.Random.Random123.RandomGen


test_gen32 = (avg < 0.55 && avg > 0.45) @?= True where
    gen = mkCBRNG32 123456
    rlist = take 10000 (randoms gen :: [Float])
    avg = sum rlist / 10000

test_gen64 = (avg < 0.55 && avg > 0.45) @?= True where
    gen = mkCBRNG64 123456
    rlist = take 10000 (randoms gen :: [Float])
    avg = sum rlist / 10000


test_randomgen = testGroup "RandomGen" [
    testCase "Default 32-bit" test_gen32,
    testCase "Default 64-bit" test_gen64
    ]
