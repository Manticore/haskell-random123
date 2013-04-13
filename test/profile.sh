#!/bin/bash
rm profile
ghc -i../ -prof -fprof-auto -rtsopts -O2 profile.hs
./profile +RTS -p

cat profile.prof

rm profile
ghc -i../ -O2 profile.hs
time ./profile
