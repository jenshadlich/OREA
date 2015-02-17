{-# language FlexibleContexts #-}

module UnitTests.Genetic.Threshold where

import UnitTests.Assert
import UnitTests.Genetic.Helper
import Genetic.Types

--------------------------------------------------------------------------------
test1 = do
    result <- runEvolve (Tournament 0.5 3) (Best100) (Just 0)
    assertEqual "check length" True (10 == (length $ population result))
    return ()

--------------------------------------------------------------------------------
-- | Main
main = do
    test1
    return () 
