{-# LANGUAGE FlexibleInstances #-}

-- |
module System.Random.Random123 (
    -- * Default RNGs
    CBRNG32,
    mkCBRNG32,
    CBRNG64,
    mkCBRNG64,
    -- * Custom RNGs
    mkCustomCBRNG32,
    mkCustomCBRNG64,
    -- * Keyed bijection functions
    philox2,
    philox4,
    threefry2,
    threefry4
    ) where

import Data.Word
import Data.Bits
import System.Random

import System.Random.Random123.Philox
import System.Random.Random123.Threefry
import System.Random.Random123.RandomGen
