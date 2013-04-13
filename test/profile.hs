import Data.Word

import System.Random.Random123.Philox
import System.Random.Random123.Threefry
import System.Random.Random123.Misc


a1x32 = 123 :: Word32
a2x32 = (123 :: Word32, 456 :: Word32)
a4x32 = (123 :: Word32, 456 :: Word32, 789 :: Word32, 101112 :: Word32)
a1x64 = 123 :: Word64
a2x64 = (123 :: Word64, 456 :: Word64)
a4x64 = (123 :: Word64, 456 :: Word64, 789 :: Word64, 101112 :: Word64)

threefry2x32 = (threefry2 a2x32, a2x32)
threefry4x32 = (threefry4 a4x32, a4x32)
threefry2x64 = (threefry2 a2x64, a2x64)
threefry4x64 = (threefry4 a4x64, a4x64)

philox2x32 = (philox2 a1x32, a2x32)
philox4x32 = (philox4 a2x32, a4x32)
philox2x64 = (philox2 a1x64, a2x64)
philox4x64 = (philox4 a2x64, a4x64)

iterations = 1000
testFunc = threefry2x32

main :: IO ()
main = print $ apply (fst testFunc) iterations (snd testFunc)
