module Isogram (isIsogram) where

import qualified Data.Set as Set
import Data.Char (toLower)   
isIsogram :: String -> Bool
isIsogram text = go Set.empty lowerText
  where
    lowerText = map toLower text
    go _ [] = True
    go seen (x:xs)
      | x == '-' || x == ' ' = go seen xs
      | x `Set.member` seen = False
      | otherwise = go (Set.insert x seen) xs
