module TestThreefry (test_threefry) where

import Data.Word

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import System.Random.Random123.Types
import System.Random.Random123.Threefry


-- Reference results were obtained using Random123 v1.07
-- (with a minor bug fixed --- rotation constant order is broken in 20-th round)


-- Checking reference results for Threefry-2x32-32

test_threefry2x32 = results @?= reference where
    key = (123, 456) :: Array2 Word32
    rounds = 32
    counters = [
        (0x058741f8, 0x346b43e7),
        (0xcedc5891, 0x2c5c1d28),
        (0x5a086b94, 0x1acbdc33),
        (0xa76cc7bf, 0x857c8f0f),
        (0xad8d9e57, 0x01b1d9fa)]
    reference = [
        (0x2d4c5ff6, 0x76375d6e),
        (0x78b73f35, 0xea905a45),
        (0xfde30fed, 0x9c104974),
        (0x15861c3d, 0x293ba61f),
        (0x5033c8d1, 0x7b92151a)]
    results = map (threefry2R rounds key) counters


-- Checking reference results for Threefry-4x32-72

test_threefry2x64 = results @?= reference where
    key = (123, 456, 789, 101112) :: Array4 Word32
    rounds = 72
    counters = [
        (0x589c8db6, 0x19d662ad, 0x3e63b1e5, 0x25dad2c5),
        (0x403512be, 0x370c9a86, 0x66d4c418, 0x92ed3769),
        (0x1920eb77, 0x2d6340fe, 0xa2f618e3, 0xb8e4aacf),
        (0x66005aa2, 0xf70ecfdf, 0xe38e29c5, 0xcbbd54d2),
        (0x295e45f0, 0xa3d266cd, 0xb102152d, 0x2f57b507)]
    reference = [
        (0xcc07f462, 0x18cbe434, 0x97b800fe, 0xc515a3c1),
        (0x67bf63ec, 0x08274d21, 0x18557796, 0x36706c15),
        (0x428a06be, 0xbf1caad7, 0x25e51e44, 0x89e4bd74),
        (0xbfff9163, 0x4106e304, 0x03157e56, 0xebfbd11c),
        (0x5a1f4a9c, 0x1725d096, 0x2b561b17, 0xeb9b4807)]
    results = map (threefry4R rounds key) counters


-- Checking reference results for Threefry-2x64-32

test_threefry4x32 = results @?= reference where
    key = (123, 456) :: Array2 Word64
    rounds = 32
    counters = [
        (0x097a4f5083c77380, 0x51794d6039c75400),
        (0x65918ba6ccfe6400, 0x3d91c69276916c00),
        (0xdfe3269c92f1f800, 0xe6a0ab67f2207800),
        (0x6240b418d521f000, 0x40299ed82854f400),
        (0x9acc98590f691000, 0x89df228bd6bae800)]
    reference = [
        (0xa6102e76d20f3382, 0x44dbf36431f9e9ee),
        (0x0da48a45f9295d0e, 0x95d3b41c67e0f4c2),
        (0x050a8dfccae89041, 0x76c474fad535292d),
        (0xea84579be191a8fc, 0x3236c9527cba73e9),
        (0x31f9c11860715320, 0x7ba946b5076efd08)]
    results = map (threefry2R rounds key) counters


-- Checking reference results for Threefry-4x64-72

test_threefry4x64 = results @?= reference where
    key = (123, 456, 789, 101112) :: Array4 Word64
    rounds = 72
    counters = [
        (0x916c0b160f927800, 0x52eb051cf7d14400, 0xbb685306834af800, 0x472bc9ebd7c67000),
        (0x61b7386b8fea1800, 0x8e3c63aaaf6f0000, 0x32a99c88983d4400, 0x0e6631cb0b255a00),
        (0x5ac82c4303a38400, 0xe3b2d6d630f00000, 0xb0fd6567c0918800, 0x103c4caced3ad200),
        (0x8df94ae6b44d9000, 0x6a20b71e020c1800, 0x7a7554df0c355000, 0x0ab535e581250880),
        (0x2f8ec42369683200, 0xda1197831643b000, 0xe1e589e32f32c800, 0x0b7a9b0053df1480)]
    reference = [
        (0x8d72c51e3e02c62c, 0xf343d5bdd4428602, 0xc3769f920f6bb846, 0x68f81b2a1276a4e1),
        (0x333f98bad6286630, 0xbd1330b5f841bc3a, 0x95f3aab12571ae68, 0xbc2f487dd4b9d63c),
        (0xebec496e8c1198ab, 0xdd7ea52e93dbf166, 0x6894711ca28c7c12, 0x5817ba9c9f36f1bc),
        (0x892f196d93952843, 0x4341d023d4fe64f3, 0x72bd9fba2c6ed9c3, 0x5f7affd1dd0b449f),
        (0xc91f23d22b34be06, 0x8ddbf89963b13e67, 0x230bb28bfb3f4b3c, 0xefaa712baaa3dafb)]
    results = map (threefry4R rounds key) counters


test_threefry = testGroup "Threefry, comparison with the reference" [
    testCase "2x32" test_threefry2x32,
    testCase "4x32" test_threefry4x32,
    testCase "2x64" test_threefry2x64,
    testCase "4x64" test_threefry4x64
    ]
