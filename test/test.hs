import Test.Framework (defaultMain)

import TestThreefry (test_threefry)
import TestPhilox (test_philox)
import TestTypes (test_types)
import TestRandomGen (test_randomgen)

tests = [
    test_threefry,
    test_philox,
    test_types,
    test_randomgen
    ]

main = defaultMain tests
