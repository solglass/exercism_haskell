module RotationalCipher (rotate) where

import Data.Char (chr, isAsciiUpper, isAsciiLower, ord)

rotate :: Int -> String -> String
rotate n = map (rotateChar n)

rotateChar :: Int -> Char -> Char
rotateChar n c
  | isAsciiUpper c = shift 'A' c
  | isAsciiLower c = shift 'a' c
  | otherwise      = c
  where
    shift base ch = chr (ord base + (ord ch - ord base + n) `mod` 26)
