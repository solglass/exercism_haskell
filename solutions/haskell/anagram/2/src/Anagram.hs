module Anagram (anagramsFor) where
import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [s | s <- xss, isAnagram xs s]
  where
    isAnagram :: String -> String -> Bool
    isAnagram ys zs = (sort (map toLower ys) == sort (map toLower zs)) 
        && (map toLower ys /= map toLower zs)
