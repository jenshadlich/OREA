
module Util.Random where

import System.Random

--------------------------------------------------------------------------------
-- | Zufallszahl aus dem gegebenen Bereich.
getRandomNumber :: (Int, Int)
                -> IO Int
getRandomNumber (l, r) = System.Random.randomRIO (l, r)

--------------------------------------------------------------------------------
-- | Aus einer Liste ein zufaelliges Element auswaehlen.
one :: [a] -> IO (a)
one xs = do
    r <- getRandomNumber (0, (length (xs)-1))
    let x = xs!!r
    return x
