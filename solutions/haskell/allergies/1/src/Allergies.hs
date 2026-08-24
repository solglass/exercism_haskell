module Allergies (Allergen(..), allergies, isAllergicTo) where

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

allergenScore :: Allergen -> Int
allergenScore allergen = 2 ^ fromEnum allergen

allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo` score) [minBound .. maxBound]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score `div` allergenScore allergen) `mod` 2 == 1
