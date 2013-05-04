-- | Test standard RandomGen interface of Random123-backed generators.
module TestTypeclasses (test_typeclasses) where

import Data.Word

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import System.Random.Random123.Types


test_li1x32_bijection initial = initial == mapped initial where
    types = initial :: Word32
    mapped = liFromInteger . liToInteger

test_li2x32_bijection initial = initial == mapped initial where
    types = initial :: Array2 Word32
    mapped = liFromInteger . liToInteger

test_li4x32_bijection initial = initial == mapped initial where
    types = initial :: Array4 Word32
    mapped = liFromInteger . liToInteger

test_li1x64_bijection initial = initial == mapped initial where
    types = initial :: Word64
    mapped = liFromInteger . liToInteger

test_li2x64_bijection initial = initial == mapped initial where
    types = initial :: Array2 Word64
    mapped = liFromInteger . liToInteger

test_li4x64_bijection initial = initial == mapped initial where
    types = initial :: Array4 Word64
    mapped = liFromInteger . liToInteger


test_li1x32_bitsize li = liToInteger li < 2 ^ liBitSize li where
    types = li :: Word32

test_li2x32_bitsize li = liToInteger li < 2 ^ liBitSize li where
    types = li :: Array2 Word32

test_li4x32_bitsize li = liToInteger li < 2 ^ liBitSize li where
    types = li :: Array4 Word32

test_li1x64_bitsize li = liToInteger li < 2 ^ liBitSize li where
    types = li :: Word64

test_li2x64_bitsize li = liToInteger li < 2 ^ liBitSize li where
    types = li :: Array2 Word64

test_li4x64_bitsize li = liToInteger li < 2 ^ liBitSize li where
    types = li :: Array4 Word64


test_ctr2x32_skip ctr i = liFromInteger (liToInteger ctr + i) == skip i ctr where
    types = (ctr :: Array2 Word32, i :: Integer)

test_ctr4x32_skip ctr i = liFromInteger (liToInteger ctr + i) == skip i ctr where
    types = (ctr :: Array4 Word32, i :: Integer)

test_ctr2x64_skip ctr i = liFromInteger (liToInteger ctr + i) == skip i ctr where
    types = (ctr :: Array2 Word64, i :: Integer)

test_ctr4x64_skip ctr i = liFromInteger (liToInteger ctr + i) == skip i ctr where
    types = (ctr :: Array4 Word64, i :: Integer)


test_ctr2x32_increment ctr = increment ctr == skip 1 ctr where
    types = ctr :: Array2 Word32

test_ctr4x32_increment ctr = increment ctr == skip 1 ctr where
    types = ctr :: Array4 Word32

test_ctr2x64_increment ctr = increment ctr == skip 1 ctr where
    types = ctr :: Array2 Word64

test_ctr4x64_increment ctr = increment ctr == skip 1 ctr where
    types = ctr :: Array4 Word64



test_typeclasses = testGroup "Typeclasses" [
    testGroup "LimitedInteger" [
        testGroup "liFromInteger . liToInteger == id" [
            testProperty "1x32" test_li1x32_bijection,
            testProperty "2x32" test_li2x32_bijection,
            testProperty "4x32" test_li4x32_bijection,
            testProperty "1x64" test_li1x64_bijection,
            testProperty "2x64" test_li2x64_bijection,
            testProperty "4x64" test_li4x64_bijection
            ],
        testGroup "liToInteger < 2 ^ liBitSize" [
            testProperty "1x32" test_li1x32_bitsize,
            testProperty "2x32" test_li2x32_bitsize,
            testProperty "4x32" test_li4x32_bitsize,
            testProperty "1x64" test_li1x64_bitsize,
            testProperty "2x64" test_li2x64_bitsize,
            testProperty "4x64" test_li4x64_bitsize
            ]
        ],
    testGroup "Counter" [
        testGroup "skip behaves like the default implementation" [
            testProperty "2x32" test_ctr2x32_skip,
            testProperty "4x32" test_ctr4x32_skip,
            testProperty "2x64" test_ctr2x64_skip,
            testProperty "4x64" test_ctr4x64_skip
            ],
        testGroup "increment == skip 1" [
            testProperty "2x32" test_ctr2x32_increment,
            testProperty "4x32" test_ctr4x32_increment,
            testProperty "2x64" test_ctr2x64_increment,
            testProperty "4x64" test_ctr4x64_increment
            ]
        ]
    ]
