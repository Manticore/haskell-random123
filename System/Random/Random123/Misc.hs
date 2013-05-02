{-# OPTIONS_HADDOCK hide #-}

-- | Miscellaneous helper functions.
module System.Random.Random123.Misc where


-- | Apply a function to its result sequentially,
--   additionally passing it the current iteration number.
apply :: (Int -> a -> a) -> Int -> a -> a
apply f n = applyLoop 0 where
    applyLoop i v
        | i == n = v
        | otherwise = applyLoop (i + 1) $ f i v

-- | Apply a function to its result sequentially.
apply_ :: (a -> a) -> Int -> a -> a
apply_ f = apply (\_ v -> f v)
