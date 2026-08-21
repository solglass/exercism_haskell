module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier x = (x - 10) `div` 2

ability :: Gen Int
ability = do
  rolls <- sequence $ map (const rollD6) $ replicate 4 ()
  let m = minimum rolls
      s = sum rolls
  return $ s - m

rollD6 :: Gen Int
rollD6 = choose(1,6) 

character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  chr <- ability
  return $ Character str dex con int wis chr (10 + modifier con)

