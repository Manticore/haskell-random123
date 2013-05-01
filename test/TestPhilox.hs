module TestPhilox (test_philox) where

import Data.Word

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import System.Random.Random123.Types
import System.Random.Random123.Philox


-- Checking reference results for Philox-2x32-10

test_philox2x32 = results @?= reference where
    key = 123 :: Array1 Word32
    rounds = 10
    (counters, reference) = unzip [
        ((0, 0), (0xf844fda3, 0x8471a7bc)),
        ((1, 0), (0x1c0088c6, 0xe4cea27e)),
        ((2, 0), (0xc90d7a20, 0xa64efdcc)),
        ((3, 0), (0x58e869ff, 0xcf77dad5)),
        ((4, 0), (0x4aa25904, 0x49041bf9))]
    results = map (philox2R rounds key) counters


-- Checking reference results for Philox-4x32-10

test_philox2x64 = results @?= reference where
    key = (123, 456) :: Array2 Word32
    rounds = 10
    (counters, reference) = unzip [
        ((0, 0, 0, 0), (0xd9392b80, 0x0a6a66ce, 0x010bc7c6, 0xe935bf8e)),
        ((1, 0, 0, 0), (0x4e76cd54, 0x532901b2, 0x1889a270, 0x62b708d1)),
        ((2, 0, 0, 0), (0xed888015, 0x7ff88042, 0x6cdd14da, 0xbc9a0c2f)),
        ((3, 0, 0, 0), (0xa7de2b2e, 0xba57e68a, 0xe556b0fc, 0xbb1d6d0e)),
        ((4, 0, 0, 0), (0x74f6a1ed, 0xcf62b2b6, 0x75639da9, 0x75191bda))]
    results = map (philox4R rounds key) counters


-- Checking reference results for Philox-2x64-10

test_philox4x32 = results @?= reference where
    key = 123 :: Word64
    rounds = 10
    (counters, reference) = unzip [
        ((0, 0), (0xb22df93e9c84bfc5, 0xe0ea6a0a15f83873)),
        ((1, 0), (0x128cf138d8d9060c, 0x4f526c315be5297d)),
        ((2, 0), (0xae5a8a95e1d0b401, 0x69e09437eba9102c)),
        ((3, 0), (0xb91ca19300f0a886, 0x449e73bb22afd2b7)),
        ((4, 0), (0xd592e15a3fc37223, 0xeac7eca148f7cc1e))]
    results = map (philox2R rounds key) counters


-- Checking reference results for Philox-4x64-10

test_philox4x64 = results @?= reference where
    key = (123, 456) :: Array2 Word64
    rounds = 10
    (counters, reference) = unzip [
        ((0, 0, 0, 0), (0x3cdcad1fb4763de7, 0xf94cc91d1fab146b, 0x3aa7490df501df51, 0x458f65a8c046a6fa)),
        ((1, 0, 0, 0), (0x182a33ef112a55c6, 0x7fa21420170db5b7, 0x3d065f703e33bef6, 0x29ec19a7d6e63a9a)),
        ((2, 0, 0, 0), (0x280c361dcea8055e, 0x0de096e158cf6aa5, 0x47958d4675ff3bfa, 0xdebe3ce8f8da523e)),
        ((3, 0, 0, 0), (0xab85413121df79f0, 0x034ac8f5f81151ed, 0xf8689591c201eab8, 0xdaa456ec24eb937e)),
        ((4, 0, 0, 0), (0x45fe5ec993667f29, 0x392a9d088bdaf5b0, 0xa09f0622332af717, 0xccee2dda90f6d9f7))]
    results = map (philox4R rounds key) counters


test_philox = testGroup "Philox" [
    testCase "2x32" test_philox2x32,
    testCase "4x32" test_philox4x32,
    testCase "2x64" test_philox2x64,
    testCase "4x64" test_philox4x64
    ]
