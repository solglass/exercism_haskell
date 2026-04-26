module Bob (responseFor) where

import Data.Char (isUpper, isLetter, isSpace)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor query
  | all isSpace query || null query   = "Fine. Be that way!"
  | isShouting && isQuestion = "Calm down, I know what I'm doing!"
  | isShouting               = "Whoa, chill out!"
  | isQuestion               = "Sure."
  | otherwise                   = "Whatever."
  where
    isShouting = any isLetter query && all isUpper (filter isLetter query)
    isQuestion = not (null stripped) && last stripped == '?'
      where stripped = dropWhileEnd isSpace query