import Test.Framework (defaultMain)

import TestThreefry (test_threefry)
import TestPhilox (test_philox)
import TestTypeclasses (test_typeclasses)
import TestRandomGen (test_randomgen)

tests = [
    test_threefry,
    test_philox,
    test_typeclasses,
    test_randomgen
    ]

main :: IO ()
main = defaultMain tests
