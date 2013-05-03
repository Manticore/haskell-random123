import Data.Word

import System.Random

import System.Random.Random123.Misc
import System.Random.Random123.Types
import System.Random.Random123


a1x32 = 123 :: Word32
a2x32 = (123, 456) :: Array2 Word32
a4x32 = (123, 456, 789, 101112) :: Array4 Word32
a1x64 = 123 :: Word64
a2x64 = (123, 456) :: Array2 Word64
a4x64 = (123, 456, 789, 101112) :: Array4 Word64

iterations = 100000

-- Use to profile bijections

threefry2x32 = (threefry2 a2x32, a2x32)
threefry4x32 = (threefry4 a4x32, a4x32)
threefry2x64 = (threefry2 a2x64, a2x64)
threefry4x64 = (threefry4 a4x64, a4x64)
philox2x32 = (philox2 a1x32, a2x32)
philox4x32 = (philox4 a2x32, a4x32)
philox2x64 = (philox2 a1x64, a2x64)
philox4x64 = (philox4 a2x64, a4x64)

test_bijection test = apply_ (fst test) iterations (snd test)

-- Use to profile RNGs
test_rng = sum (take iterations (randoms (mkCBRNG32 0) :: [Float]))

main :: IO ()
main = do
    print test_rng
    print $ test_bijection threefry2x32
    print $ test_bijection philox2x32
    print $ test_bijection threefry4x32
    print $ test_bijection philox4x32
    print $ test_bijection threefry2x64
    print $ test_bijection philox2x64
    print $ test_bijection threefry4x64
    print $ test_bijection philox4x64
