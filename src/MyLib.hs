
module MyLib where

import System.Random

myArray :: [Int]
myArray = randomRs (1, 6) (mkStdGen 3)






