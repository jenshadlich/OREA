{-# language FlexibleContexts #-}

module UnitTests.Genetic.Selections where

import UnitTests.Genetic.Helper
import UnitTests.Assert
import Genetic.Types

--------------------------------------------------------------------------------
testSelections parSel envSel = do
    result <- runEvolve parSel envSel Nothing
    assertEqual "check length" True (10 <= (length $ population result))
    return ()

--------------------------------------------------------------------------------
-- | Main
main = do
    testSelections (Tournament 0.5 3) (Best100Unique)
    testSelections (Tournament 0.5 3) (Best100)
    testSelections (Tournament 0.5 3) (NAryDoubleTournament 3)
    testSelections (Tournament 0.5 3) (Best80Random20)
    testSelections (FitnessProp 0.5)  (Best100)
    testSelections (FitnessPropWithTournament 0.5 2) (Best100)
    testSelections (Linear 0.5) (Best100)
    return () 
