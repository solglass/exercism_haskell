module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs 
    | null xs   = []
    | p x       = discard p xs'
    | otherwise = x : discard p xs'
    where
        x:xs' = xs

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [x | x <- xs, p x]
