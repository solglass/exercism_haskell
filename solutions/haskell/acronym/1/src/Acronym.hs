module Acronym (abbreviate) where
import qualified Data.Text as T 
import Data.Char (isAlpha, toUpper, isUpper)

normalize :: T.Text -> T.Text
normalize = T.map (\c -> if isAlpha c || c == '\'' then c else ' ')

abbreviate :: String -> String
abbreviate = foldMap firstLetter . T.words . normalize . T.pack
 where
    firstLetter :: T.Text -> String
    firstLetter t
        | countCapitals t > 1 && countNonCapitals t > 0  = T.unpack (T.filter isUpper t)
        | otherwise           = map toUpper (take 1 (T.unpack (T.filter isAlpha t)))
        
    countCapitals :: T.Text -> Int
    countCapitals = T.length . T.filter isUpper

    countNonCapitals :: T.Text -> Int
    countNonCapitals = T.length . T.filter (not . isUpper)


