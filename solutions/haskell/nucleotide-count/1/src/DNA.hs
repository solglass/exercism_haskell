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

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts s 
    | null s = Right $ Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    | isValidNucleotide s = Right $ foldr countNucleotide Map.empty s
    | otherwise = Left "Invalid nucleotide found"
    where
        countNucleotide :: Char -> Map Nucleotide Int -> Map Nucleotide Int
        countNucleotide c counts = case c of
            'A' -> Map.insertWith (+) A 1 counts
            'C' -> Map.insertWith (+) C 1 counts
            'G' -> Map.insertWith (+) G 1 counts
            'T' -> Map.insertWith (+) T 1 counts
            _   -> counts -- This case should never happen due to validation

