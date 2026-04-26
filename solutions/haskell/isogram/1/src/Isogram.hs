module Isogram (isIsogram) where
import Data.Char (toLower)   

isIsogram :: String -> Bool
isIsogram text = go [] lowerText
  where
    lowerText = map toLower text
    go _ [] = True
    go seen (x:xs)
      | x == '-' || x == ' ' = go seen xs
      | x `elem` seen = False
      | otherwise = go (x:seen) xs
