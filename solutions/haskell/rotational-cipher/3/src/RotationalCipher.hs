module RotationalCipher (rotate, rotateWith) where

import Data.Char (chr, isAsciiUpper, isAsciiLower, isDigit, ord)

-- | Classic rotational cipher: shifts letters by @n@, leaves digits alone.
rotate :: Int -> String -> String
rotate n = rotateWith n 0

-- | Rotational cipher with independent shifts for letters and digits,
-- e.g. @rotateWith 13 5@ applies ROT13 to letters and ROT5 to digits.
rotateWith :: Int -> Int -> String -> String
rotateWith letterShift digitShift = map (rotateChar letterShift digitShift)

rotateChar :: Int -> Int -> Char -> Char
rotateChar letterShift digitShift c
  | isAsciiUpper c = shift 'A' 26 letterShift c
  | isAsciiLower c = shift 'a' 26 letterShift c
  | isDigit c      = shift '0' 10 digitShift c
  | otherwise      = c
  where
    shift base modulus n ch = chr (ord base + (ord ch - ord base + n) `mod` modulus)
