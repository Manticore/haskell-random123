module TestThreefry (test_threefry) where

import Data.Word

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import System.Random.Random123.Threefry


type Array2x32 = (Word32, Word32)
type Array4x32 = (Word32, Word32, Word32, Word32)
type Array2x64 = (Word64, Word64)
type Array4x64 = (Word64, Word64, Word64, Word64)

threefry2x32key = (123, 456) :: Array2x32
threefry4x32key = (123, 456, 789, 101112) :: Array4x32
threefry2x64key = (123, 456) :: Array2x64
threefry4x64key = (123, 456, 789, 101112) :: Array4x64

threefry2x32results :: [(Array2x32, Array2x32)]
threefry2x32results = [
    ((0, 0), (0x44fa0a46, 0xee68d198)),
    ((1, 0), (0x5d7c148b, 0x1b5642f9)),
    ((2, 0), (0xda135215, 0x9ff4dc46)),
    ((3, 0), (0xa659b4bf, 0x5cfa153b)),
    ((4, 0), (0xa183bd9e, 0x06ce511e))
    ]

threefry4x32results :: [(Array4x32, Array4x32)]
threefry4x32results = [
    ((0, 0, 0, 0), (0xc67a15ff, 0x1f294109, 0x18a392cd, 0x901ff30c)),
    ((1, 0, 0, 0), (0x849a55f4, 0x23948454, 0x9cc01d9a, 0xf610ec3e)),
    ((2, 0, 0, 0), (0x59ced9b5, 0x22d44d98, 0x9a2e6b4c, 0x8a6d428f)),
    ((3, 0, 0, 0), (0x81b5a49e, 0x5a2aff79, 0x5da97cc5, 0x774929fd)),
    ((4, 0, 0, 0), (0x543952ef, 0x9ac89c2d, 0x9f2a6a6a, 0x7dab5f5f))
    ]

threefry2x64results :: [(Array2x64, Array2x64)]
threefry2x64results = [
    ((0, 0), (0x2f94adcdbf5e99b0, 0x8ceec9683bbd82cc)),
    ((1, 0), (0xe27c9db97d68bfeb, 0x0509d05b85fe1752)),
    ((2, 0), (0x790eca0901fd3d96, 0x8a2bdd2096be6ff7)),
    ((3, 0), (0x0e08a49c8f398236, 0xe8e60680e60e8c27)),
    ((4, 0), (0x31448e1bce8f6d81, 0xba45258cb93f2aa5))
    ]

threefry4x64results :: [(Array4x64, Array4x64)]
threefry4x64results = [
    ((0, 0, 0, 0), (0x92c90c3543470fd7, 0xcbbdc0c2672a29d3, 0x4c72604bb3049e52, 0x2fc833631da64583)),
    ((1, 0, 0, 0), (0x9ca0c9ec4e63fd6a, 0x1f9c5471ba5ddcfc, 0x7497d8d9f036a812, 0x90dfcf3afd0bedf5)),
    ((2, 0, 0, 0), (0xb868ddbd7959bcc1, 0xf72e76dd0520087a, 0x284b40cc68956c0e, 0x84f883b34173599d)),
    ((3, 0, 0, 0), (0xfbaa3c5e47f504e7, 0x6f2649add2a7711b, 0x6a5cb2e2376756e1, 0xbd5e8aa4af1258a6)),
    ((4, 0, 0, 0), (0xe984b393bc787737, 0x884b7393f32ae770, 0x3185c98d31175081, 0xb93d4c8d1a0d0b7f))
    ]


test_threefry2x32 = results @?= reference where
    (counters, reference) = unzip threefry2x32results
    results = map (threefry2R 32 threefry2x32key) counters

test_threefry4x32 = results @?= reference where
    (counters, reference) = unzip threefry4x32results
    results = map (threefry4R 72 threefry4x32key) counters

test_threefry2x64 = results @?= reference where
    (counters, reference) = unzip threefry2x64results
    results = map (threefry2R 32 threefry2x64key) counters

test_threefry4x64 = results @?= reference where
    (counters, reference) = unzip threefry4x64results
    results = map (threefry4R 72 threefry4x64key) counters


test_threefry = testGroup "Threefry" [
    testCase "2x32" test_threefry2x32,
    testCase "4x32" test_threefry4x32,
    testCase "2x64" test_threefry2x64,
    testCase "4x64" test_threefry4x64
    ]
