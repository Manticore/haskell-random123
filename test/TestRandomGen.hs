-- | Test standard RandomGen interface of Random123-backed generators.
module TestRandomGen (test_randomgen) where

import Data.Word

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import System.Random.Random123.Types


test_philox4x64 = results @?= reference where
    --key = (123, 456) :: Array4 Word64
    --gen = mkRandom123Gen philox4 key ctr
    reference = 1
    results = 1


test_randomgen = testGroup "RandomGen" [
    testCase "Philox-4x64" test_philox4x64
    ]
