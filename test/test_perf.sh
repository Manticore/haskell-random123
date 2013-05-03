#!/bin/bash
rm test_perf
ghc -i../ -O2 test_perf.hs
./test_perf -g -o test_perf.html
