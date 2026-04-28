module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = 
    sum [x | x <- [0..limit-1], isMultipleOfAnyFactor x]
      where isMultipleOfAnyFactor x = 
             any (\f -> f /= 0 && x `mod` f == 0) factors
