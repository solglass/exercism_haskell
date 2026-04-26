module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA [] = Right ""
toRNA (x:xs) = case x of
    'C' -> fmap ('G':) (toRNA xs)
    'G' -> fmap ('C':) (toRNA xs)
    'T' -> fmap ('A':) (toRNA xs)
    'A' -> fmap ('U':) (toRNA xs)
    _   -> Left x
