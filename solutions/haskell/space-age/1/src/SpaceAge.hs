module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = case planet of
    Mercury -> seconds / 0.2408467 / 31557600
    Venus   -> seconds / 0.61519726 / 31557600
    Earth   -> seconds / 1.0 / 31557600
    Mars    -> seconds / 1.8808158 / 31557600
    Jupiter -> seconds / 11.862615 / 31557600
    Saturn  -> seconds / 29.447498 / 31557600
    Uranus  -> seconds / 84.016846 / 31557600
    Neptune -> seconds / 164.79132 / 31557600
    
