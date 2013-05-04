{-# LANGUAGE FlexibleInstances #-}

-- | This module is a Haskell port of the Random123 library
-- (<http://www.thesalmons.org/john/random123/>).
-- It is based on counter-based pseudo-random number generators (CBRNGs), which are, essentially,
-- keyed bijections which transform successive counters into randomly distributed integers.
-- For details about the theory behind the algorithms along with statistical and performance tests
-- see the paper Salmon et al., P. Int. C. High. Perform. 16 (2011)
-- (<http://dx.doi.org/doi:10.1145/2063384.2063405>).
--
-- The module exposes both bijection functions themselves (for customized approach) and
-- instances of 'RandomGen'.
--
-- Since CBRNGs are based on bijection functions, their periods are equal to the size of their
-- corresponding counters.
-- For example, 32-bit 'philox4' has 'Array4' 'Word32' counter,
-- therefore the total counter size is @4 * 32 = 128@ bit, and the period is @2^128@.
--
-- 'RandomGen' instances use each generated random array for several random integers,
-- so their periods are several times bigger.
-- Consider now that the 'philox4' bijection was used to create a 'CustomCBRNG64' generator.
-- For each 64-bit 'Int' its 'next' function returns,
-- it will use two of the elements of the 'Array4' 'Word32',
-- so the total period is @2 * 2^128 = 2^129@.
--
-- /Note:/ There is no point in creating 64-bit RNGs when your platform has only 32-bit 'Int's.
-- The remaining bits will be truncated by 'next'.
module System.Random.Random123 (
    -- * Default RNGs
    -- | Use these if you want the (usually) optimal bijection algorithm,
    -- and serialization capabilities.
    CBRNG32,
    mkCBRNG32,
    restoreCBRNG32,
    CBRNG64,
    mkCBRNG64,
    restoreCBRNG64,

    -- * Custom RNGs
    -- | Use these if you want a custom bijection algorithm.
    CustomCBRNG32,
    mkCustomCBRNG32,
    restoreCustomCBRNG32,
    CustomCBRNG64,
    mkCustomCBRNG64,
    restoreCustomCBRNG64,

    -- * Keyed bijection functions
    -- | Use these if you want the ultimate control over keys and counters.
    -- Sometimes it is advantageous to bind part of the counter or the key
    -- the node or thread identifier, or to indices of a time or space grid.
    -- You can use 'liFromInteger' (or just a tuple constructor) to create keys and counters,
    -- and 'skip' and 'increment' to change counter values.
    --
    -- If you want further control over the number of rounds in these bijections,
    -- see "System.Random.Random123.Philox" and "System.Random.Random123.Threefry" modules.
    philox2,
    philox4,
    threefry2,
    threefry4
    ) where

import Data.Word
import Data.Bits
import System.Random

import System.Random.Random123.Types
import System.Random.Random123.Philox
import System.Random.Random123.Threefry
import System.Random.Random123.RandomGen
