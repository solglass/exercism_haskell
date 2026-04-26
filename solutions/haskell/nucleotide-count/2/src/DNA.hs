module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

allowed :: Set.Set Char
allowed = Set.fromList ['A', 'C', 'G', 'T']

isValidNucleotide :: String-> Bool
isValidNucleotide =T.all (`Set.member` allowed) . T.pack

{-Data.Text is an overkill for a String operation. 
Using T.count just to avoid writing a fold is pulling in a heavy package.
T.count is used 4 times VS Single pass of foldr traverses the string once.
The T.count version is more readable and is interesing to use 
but Data.Text is designed for Unicode-heavy text processing, 
not character counting in a small alphabet.
-}

countNucleotide :: String -> Char -> Int
countNucleotide nucstr nuc = T.count (T.singleton nuc) (T.pack nucstr)


nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts s 
    | null s = Right $ Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    | isValidNucleotide s = Right $ Map.fromList
        [ (A, countNucleotide s 'A')
        , (C, countNucleotide s 'C')
        , (G, countNucleotide s 'G')
        , (T, countNucleotide s 'T')
        ]
    | otherwise = Left "Invalid nucleotide found"
