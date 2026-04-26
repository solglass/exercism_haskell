module ReverseString (reverseString) where

reverseString :: String -> String
reverseString str = case str of
    [] -> []
    (x:xs) -> reverseString xs ++ [x]
