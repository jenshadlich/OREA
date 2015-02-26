
module Util.Random where

import System.Random

--------------------------------------------------------------------------------
-- | Random number from a given range
getRandomNumber :: (Int, Int)
                -> IO Int
getRandomNumber (l, r) = System.Random.randomRIO (l, r)

--------------------------------------------------------------------------------
-- | Choose a random element for the given list
one :: [a] -> IO (a)
one xs = do
    r <- getRandomNumber (0, (length (xs)-1))
    let x = xs!!r
    return x
