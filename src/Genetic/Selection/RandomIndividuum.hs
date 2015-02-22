
module Genetic.Selection.RandomIndividuum where

import System.Random (randomRIO)
import Genetic.Types

--------------------------------------------------------------------------------
-- | Zufaellig (gleichverteilt) ein Individuum aus der Liste der Individuen
-- auswaehlen.
randomIndividuum :: [Individuum gt f]
                 -> IO (Individuum gt f)
randomIndividuum is = do i <- pick is; return i

--| pick a random element from a list
pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)
