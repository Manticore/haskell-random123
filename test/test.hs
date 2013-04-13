import Test.Framework (defaultMain)

import TestThreefry (test_threefry)
import TestPhilox (test_philox)

tests = [
    test_threefry,
    test_philox
    ]

main = defaultMain tests
