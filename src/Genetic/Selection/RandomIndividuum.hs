
module Genetic.Selection.RandomIndividuum where

import Autolib.Util.Zufall (eins)
import Genetic.Types

--------------------------------------------------------------------------------
-- | Zufaellig (gleichverteilt) ein Individuum aus der Liste der Individuen
-- auswaehlen.
randomIndividuum :: [Individuum gt f]
                 -> IO (Individuum gt f)
randomIndividuum is = do i <- eins is; return i
