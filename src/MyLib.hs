
module MyLib (rollDiceWithTimeSeed) where

import System.Random
import Data.Time.Clock.POSIX


getCurrentTimeSeed :: IO Int
getCurrentTimeSeed = round <$> getPOSIXTime

-- Define a pure function to simulate rolling a dice with a time-based seed
rollDiceWithTimeSeed :: IO Int
rollDiceWithTimeSeed = do
    fst . randomR (1, 6) . mkStdGen <$> getCurrentTimeSeed


