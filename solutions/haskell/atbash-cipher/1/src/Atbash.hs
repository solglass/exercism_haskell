module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (intercalate, unfoldr)

decode :: String -> String
decode = transcode

encode :: String -> String
encode = intercalate " " . chunksOf 5 . transcode

transcode :: String -> String
transcode = map atbash . filter (\c -> isAlpha c || isDigit c)

atbash :: Char -> Char
atbash c
  | isDigit c = c
  | otherwise = chr (ord 'a' + (ord 'z' - ord (toLower c)))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr step
  where
    step [] = Nothing
    step xs = Just (splitAt n xs)
