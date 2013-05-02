{-# LANGUAGE FlexibleInstances #-}

module System.Random.Random123 (
    philox2,
    philox4,
    threefry2,
    threefry4,
    mkCustomCBRNG32,
    mkCustomCBRNG64,
    mkCBRNG32,
    mkCBRNG64
    ) where

import Data.Word
import Data.Bits
import System.Random

import System.Random.Random123.Philox
import System.Random.Random123.Threefry
import System.Random.Random123.RandomGen
