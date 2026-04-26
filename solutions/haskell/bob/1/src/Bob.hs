module Bob (responseFor) where

import Data.Char (isUpper, isLetter, isSpace)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | all isSpace xs || null xs   = "Fine. Be that way!"
  | isShouting xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isShouting xs               = "Whoa, chill out!"
  | isQuestion xs               = "Sure."
  | otherwise                   = "Whatever."
  where
    isShouting ys = any isLetter ys && all isUpper (filter isLetter ys)
    isQuestion ys = not (null stripped) && last stripped == '?'
      where stripped = dropWhileEnd isSpace ys