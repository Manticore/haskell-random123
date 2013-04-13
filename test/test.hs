import Test.Framework (defaultMain)

import TestThreefry (test_threefry)

tests = [
    test_threefry
    ]

main = defaultMain tests
