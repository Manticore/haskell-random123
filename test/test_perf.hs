import Data.Word

import Criterion.Main
import Criterion.Config

import System.Random.Random123.Threefry
import System.Random.Random123.Misc
import System.Random.Random123.Types
import System.Random.Random123
import System.Random

tftest = apply_ (threefry2R 20 (123 :: Word32, 456 :: Word32)) 1000

testfunc rng = sum (take 10000 (randoms rng :: [Float]))

test_rng_default 32 key = testfunc (mkCBRNG32 key)
test_rng_default 64 key = testfunc (mkCBRNG64 key)

data Bijection = Philox4x32 | Philox4x64 | Threefry4x32 | Threefry4x64

key2x32 key = liFromInteger key :: Array2 Word32
key2x64 key = liFromInteger key :: Array2 Word64
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


main = defaultMainWith defaultConfig (return ()) [
    bgroup "threefry" [
        bench "2x32-20" $ nf tftest (1 :: Word32, 2 :: Word32)
        ],
    bgroup "RNG" [
        bench "Default 32-bit" $ nf (test_rng_default 32) 123456,
        bench "Default 64-bit" $ nf (test_rng_default 64) 123456,
        bench "Philox4x32 + CBRNG32" $ nf (test_rng_custom 32 Philox4x32) 123456,
        bench "Philox4x64 + CBRNG32" $ nf (test_rng_custom 32 Philox4x64) 123456,
        bench "Philox4x32 + CBRNG64" $ nf (test_rng_custom 64 Philox4x32) 123456,
        bench "Philox4x64 + CBRNG64" $ nf (test_rng_custom 64 Philox4x64) 123456,
        bench "Threefry4x32 + CBRNG32" $ nf (test_rng_custom 32 Threefry4x32) 123456,
        bench "Threefry4x64 + CBRNG32" $ nf (test_rng_custom 32 Threefry4x64) 123456,
        bench "Threefry4x32 + CBRNG64" $ nf (test_rng_custom 64 Threefry4x32) 123456,
        bench "Threefry4x64 + CBRNG64" $ nf (test_rng_custom 64 Threefry4x64) 123456
        ]
    ]
