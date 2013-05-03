#!/bin/bash
rm test_perf
rm test_perf.hi
rm test_perf.o

ghc -i../ -O2 test_perf.hs

./test_perf -g -o test_perf.html
