import Data.Word

import Criterion.Main
import Criterion.Types

import System.Random.Random123.Threefry
import System.Random.Random123.Misc
import System.Random.Random123.Types
import System.Random.Random123
import System.Random

rng_iterations = 10000
bijection_iterations = 10000

testfunc rng = sum (take rng_iterations (randoms rng :: [Float]))

test_rng_default 32 key = testfunc (mkCBRNG32 key)
test_rng_default 64 key = testfunc (mkCBRNG64 key)

data Bijection =
    Philox4x32 | Philox4x64 | Threefry4x32 | Threefry4x64 |
    Philox2x32 | Philox2x64 | Threefry2x32 | Threefry2x64

key1x32 key = liFromInteger key :: Word32
key2x32 key = liFromInteger key :: Array2 Word32
key2x64 key = liFromInteger key :: Array2 Word64
key1x64 key = liFromInteger key :: Word64
key4x32 key = liFromInteger key :: Array4 Word32
key4x64 key = liFromInteger key :: Array4 Word64

test_rng_custom 32 Philox4x32 key = testfunc (mkCustomCBRNG32 philox4 $ key2x32 key)
test_rng_custom 32 Philox4x64 key = testfunc (mkCustomCBRNG32 philox4 $ key2x64 key)
test_rng_custom 64 Philox4x32 key = testfunc (mkCustomCBRNG64 philox4 $ key2x32 key)
test_rng_custom 64 Philox4x64 key = testfunc (mkCustomCBRNG64 philox4 $ key2x64 key)
test_rng_custom 32 Threefry4x32 key = testfunc (mkCustomCBRNG32 threefry4 $ key4x32 key)
test_rng_custom 32 Threefry4x64 key = testfunc (mkCustomCBRNG32 threefry4 $ key4x64 key)
test_rng_custom 64 Threefry4x32 key = testfunc (mkCustomCBRNG64 threefry4 $ key4x32 key)
test_rng_custom 64 Threefry4x64 key = testfunc (mkCustomCBRNG64 threefry4 $ key4x64 key)


-- Normalizing iteration number to make sure every bijection
-- processes the same amount of data.
test_bijection2x32 Philox2x32 key =
    apply_ (philox2 $ key1x32 key) bijection_iterations (liFromInteger 0)
test_bijection2x32 Threefry2x32 key =
    apply_ (threefry2 $ key2x32 key) bijection_iterations (liFromInteger 0)
test_bijection4x32 Philox4x32 key =
    apply_ (philox4 $ key2x32 key) (bijection_iterations `div` 2) (liFromInteger 0)
test_bijection4x32 Threefry4x32 key =
    apply_ (threefry4 $ key4x32 key) (bijection_iterations `div` 2) (liFromInteger 0)
test_bijection2x64 Philox2x64 key =
    apply_ (philox2 $ key1x64 key) (bijection_iterations `div` 2) (liFromInteger 0)
test_bijection2x64 Threefry2x64 key =
    apply_ (threefry2 $ key2x64 key) (bijection_iterations `div` 2) (liFromInteger 0)
test_bijection4x64 Philox4x64 key =
    apply_ (philox4 $ key2x64 key) (bijection_iterations `div` 4) (liFromInteger 0)
test_bijection4x64 Threefry4x64 key =
    apply_ (threefry4 $ key4x64 key) (bijection_iterations `div` 4) (liFromInteger 0)


myConfig = defaultConfig {
    forceGC = True,
    reportFile = Just "test_perf.html" }

main = defaultMainWith myConfig [
    bgroup "Bijections" [
        bench "Philox-2x32" $ nf (test_bijection2x32 Philox2x32) 101,
        bench "Philox-4x32" $ nf (test_bijection4x32 Philox4x32) 102,
        bench "Philox-2x64" $ nf (test_bijection2x64 Philox2x64) 103,
        bench "Philox-4x64" $ nf (test_bijection4x64 Philox4x64) 104,
        bench "Threefry-2x32" $ nf (test_bijection2x32 Threefry2x32) 105,
        bench "Threefry-4x32" $ nf (test_bijection4x32 Threefry4x32) 106,
        bench "Threefry-2x64" $ nf (test_bijection2x64 Threefry2x64) 107,
        bench "Threefry-4x64" $ nf (test_bijection4x64 Threefry4x64) 108
        ],
    bgroup "32-bit RNGs" [
        bench "Default 32-bit" $ nf (test_rng_default 32) 1,
        bench "Philox-4x32" $ nf (test_rng_custom 32 Philox4x32) 2,
        bench "Philox-4x64" $ nf (test_rng_custom 32 Philox4x64) 3,
        bench "Threefry-4x32" $ nf (test_rng_custom 32 Threefry4x32) 4,
        bench "Threefry-4x64" $ nf (test_rng_custom 32 Threefry4x64) 5
        ],
    bgroup "64-bit RNGs" [
        bench "Default 64-bit" $ nf (test_rng_default 64) 6,
        bench "Philox-4x32" $ nf (test_rng_custom 64 Philox4x32) 7,
        bench "Philox-4x64" $ nf (test_rng_custom 64 Philox4x64) 8,
        bench "Threefry-4x32" $ nf (test_rng_custom 64 Threefry4x32) 9,
        bench "Threefry-4x64" $ nf (test_rng_custom 64 Threefry4x64) 10
        ]
    ]
