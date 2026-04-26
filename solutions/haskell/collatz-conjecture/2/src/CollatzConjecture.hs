{-# LANGUAGE BangPatterns #-}
module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (go 0 n)
  where
    go !steps 1 = steps
    go !steps x
      | even x    = go (steps + 1) (x `div` 2)
      | otherwise = go (steps + 1) (3 * x + 1)