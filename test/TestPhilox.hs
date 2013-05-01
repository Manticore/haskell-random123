module TestPhilox (test_philox) where

import Data.Word

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import System.Random.Random123.Types
import System.Random.Random123.Philox


-- Reference results were obtained using Random123 v1.07


-- Checking reference results for Philox-2x32-10

test_philox2x32 = results @?= reference where
    key = 123 :: Array1 Word32
    rounds = 10
    counters = [
        (0x6f55b92a, 0xab1c5ea9),
        (0x2e24118f, 0xf96cd57b),
        (0x35c2faac, 0x229e22ea),
        (0x71d82da5, 0xe7758e07),
        (0xefdadeee, 0x58849b8f)]
    reference = [
        (0xc8a74eda, 0x43748166),
        (0x0f2f49b3, 0x5440de8c),
        (0x9b0b15f0, 0x62ad33f0),
        (0x8e0d363a, 0xec949c27),
        (0xef1c72c0, 0xabcdf6f6)]
    results = map (philox2R rounds key) counters


-- Checking reference results for Philox-4x32-10

test_philox2x64 = results @?= reference where
    key = (123, 456) :: Array2 Word32
    rounds = 10
    counters = [
        (0x5bf34ff4, 0x98f9badb, 0x4f816cab, 0xe62bd835),
        (0xdfe40202, 0xdf14b7de, 0x9f66a741, 0xcf4157f7),
        (0x839ac18e, 0xa043ba06, 0x94fd2987, 0x7d176d4c),
        (0xdbba4414, 0x4a0462e2, 0x74cf99d4, 0x68d78dad),
        (0x694f3a06, 0xc113b26e, 0x6b7be5fe, 0x47e59594)]
    reference = [
        (0x01c5a89a, 0xc8783325, 0x6fcbdf46, 0x0e5e2c08),
        (0xbdfdcee8, 0x2302ddcb, 0x0c6b097d, 0xfe5b1b6d),
        (0x41852fa7, 0xca2e06e0, 0x421b06e6, 0x825db2b2),
        (0x858a0b3f, 0x9c4ee5ae, 0x409bca71, 0x6174268f),
        (0x1fb47ee3, 0x465c46e9, 0xac8a8a63, 0x770b5eb8)]
    results = map (philox4R rounds key) counters


-- Checking reference results for Philox-2x64-10

test_philox4x32 = results @?= reference where
    key = 123 :: Word64
    rounds = 10
    counters = [
        (0xf5bd87c1f90bf000, 0xfdcf5569188d8000),
        (0x1fcbd563f0fc8e00, 0x9eb6d45bd5096000),
        (0x5baaae0aecd1e000, 0x53d0329123492c00),
        (0x62ef71add3320800, 0x625c1fc53fd21000),
        (0x83d3483f4f369000, 0x395f94432bdf0200)]
    reference = [
        (0x70aa5fafe6b960f0, 0xf0296d49836ec74d),
        (0x5e6a58a58da65da7, 0xd58ae0056c4f6eb0),
        (0x79feffb4c50d7a7e, 0x586c57e69f636783),
        (0x9f0ef9aa1c02cbb3, 0x64d05e4f231859ea),
        (0x529c1fe09c72783b, 0x6a3b27e993d5fd7b)]
    results = map (philox2R rounds key) counters


-- Checking reference results for Philox-4x64-10

test_philox4x64 = results @?= reference where
    key = (123, 456) :: Array2 Word64
    rounds = 10
    counters = [
        (0x1d1fc5123d698500, 0x6a0fd3ccbc9d3800, 0xe93a590b53026000, 0x3b339cb99a06fa00),
        (0x22f53249be020e00, 0xcfbd27dc0b0dc000, 0xbe8468a6b6ac3000, 0x0428398af2cfcac0),
        (0xd07b0391ea202000, 0xc6bb23b0fb74c800, 0x62720dec16217800, 0xdd5d41a2c6c29800),
        (0x4f9ffd1a930a3800, 0x41ee8eae5f157c00, 0x1e7ac9fcae5af500, 0x9e618d09ef001000),
        (0x9712f6164d422800, 0xc04f3f2e6d87a800, 0x7e2369ed83cdac00, 0xdb986d520bfcc000)]
    reference = [
        (0xcf1ee3ae885c3928, 0x90a39b52bb92f018, 0x1e2c1e36cdafc6d5, 0x2068f5a63acafdbb),
        (0x28b86429c0cc8d2f, 0xedb2c9aeaa6ef776, 0x7615c3e5815ea96c, 0xdcf5e8849a76aec0),
        (0x775904470661a226, 0xeceefd4563c20c51, 0x5d842c642c640a6b, 0xd17f2f87ad2fa74e),
        (0x00c263e4bc04441f, 0xaecb35c835e03874, 0x46bb918e4a85adb7, 0x6c5fd577457f6315),
        (0x61ca2527c8acdb96, 0x87c6dd6227eb5256, 0x6901e9c532adea59, 0x9bd765a53cca4d5f)]
    results = map (philox4R rounds key) counters


test_philox = testGroup "Philox" [
    testCase "2x32" test_philox2x32,
    testCase "4x32" test_philox4x32,
    testCase "2x64" test_philox2x64,
    testCase "4x64" test_philox4x64
    ]
