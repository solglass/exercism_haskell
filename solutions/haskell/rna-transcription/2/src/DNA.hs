module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse nucleotide
  where
    nucleotide 'C' = Right 'G'
    nucleotide 'G' = Right 'C'
    nucleotide 'T' = Right 'A'
    nucleotide 'A' = Right 'U'
    nucleotide  x  = Left x
