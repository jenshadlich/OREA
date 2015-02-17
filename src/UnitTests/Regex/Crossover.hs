{-# language FlexibleContexts #-}

module UnitTests.Regex.Crossover where

import UnitTests.Assert

-- Dependancies
import Autolib.Exp.Type
import Autolib.NFA hiding (Dot)
import Genetic.Types
import Regex.Util

-- Module to test
import Regex.Ops.Crossover

--------------------------------------------------------------------------------
testRecombinate = do
    mapM_ (\_ -> runTest) [1..100] 
    where
        runTest = do
            m <- recombinate rx ry
            print $ show m
            m <- recombinate2 rx ry
            print $ show m
            m <- recombinate3 rx ry
            print $ show m
            assertTrue "" True
        (_, rx) = mkRX "ab" "(aa + b)^*"
        (_, ry) = mkRX "ab" "(aab(b)^*aa + aabb)^*"

--------------------------------------------------------------------------------
-- | Main
main = do
    testRecombinate
    return ()