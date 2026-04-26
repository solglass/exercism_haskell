module Darts (score) where

score :: Float -> Float -> Int
score x y = case r of
    r | r <= 1.0 -> 10
    r | r <= 5.0 -> 5
    r | r <= 10.0 -> 1
    _ -> 0
  where
    r = sqrt (x^2 + y^2)
