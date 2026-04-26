module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = all (\c -> elem c lowerText) ['a'..'z']
  where lowerText = map toLower text