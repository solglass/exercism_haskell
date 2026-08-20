module ProteinTranslation (proteins) where
import Data.List.Split (chunksOf)
data Codon = Stop | Amino String

proteins :: String -> Maybe [String]
proteins rna
    | length rna `mod` 3 /= 0 = Nothing
    | otherwise = translate (chunksOf 3 rna)
  where
    translate :: [String] -> Maybe [String]
    translate [] = Just []
    translate (c:cs) = case codonToProtein c of
        Nothing          -> Nothing
        Just Stop        -> Just []
        Just (Amino name) -> fmap (name :) (translate cs)

    codonToProtein :: String -> Maybe Codon
    codonToProtein "AUG" = Just (Amino "Methionine")
    codonToProtein "UUU" = Just (Amino "Phenylalanine")
    codonToProtein "UUC" = Just (Amino "Phenylalanine")
    codonToProtein "UUA" = Just (Amino "Leucine")
    codonToProtein "UUG" = Just (Amino "Leucine")
    codonToProtein "UCU" = Just (Amino "Serine")
    codonToProtein "UCC" = Just (Amino "Serine")
    codonToProtein "UCA" = Just (Amino "Serine")
    codonToProtein "UCG" = Just (Amino "Serine")
    codonToProtein "UAU" = Just (Amino "Tyrosine")
    codonToProtein "UAC" = Just (Amino "Tyrosine")
    codonToProtein "UGU" = Just (Amino "Cysteine")
    codonToProtein "UGC" = Just (Amino "Cysteine")
    codonToProtein "UGG" = Just (Amino "Tryptophan")
    codonToProtein "UAA" = Just Stop
    codonToProtein "UAG" = Just Stop
    codonToProtein "UGA" = Just Stop
    codonToProtein _     = Nothing
