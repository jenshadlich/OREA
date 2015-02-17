{-# language FlexibleContexts #-}

module UnitTests.Util.Analysis where

-- HUnit
import Test.HUnit

import Genetic.Trace
import Genetic.Types

-- Zu testendes Modul
import Util.Analysis

--------------------------------------------------------------------------------
mkDummyConfig = Configuration {
        maxGenerations    = 10,
        maxPopulationSize = 10,
        threshold         = Nothing,
        phenotype         = "",
        genpool           = "",
        seed              = Nothing,
        parSelection      = Tournament 0.5 2,      
        envSelection      = Best100,
        fDecode           = id,
        fFitness          = (\_ _ _ -> 1.0 :: Double),
        fTracePop         = noTrace,
        fGenerate         = (\_ -> do return ""),
        fMutate           = (\_ _ -> do return ""),
        fCrossover        = (\_ _ -> do return [])
    }

--------------------------------------------------------------------------------
mkPop is = Population {
        population = is,
        config = mkDummyConfig
    }

--------------------------------------------------------------------------------
mkIndiv g f = Individuum {
        genotype = g,
        fitness = f
    }

--------------------------------------------------------------------------------
pop1 = mkPop [mkIndiv "" 1.0, mkIndiv "" 2.0, mkIndiv "" 3.0]
pop2 = mkPop [mkIndiv "aa" 0, mkIndiv "ab" 0, mkIndiv "bbb" 0]

--------------------------------------------------------------------------------
testAvg = do TestCase $ assertEqual "" 2.0 (averageFitness pop1)

--------------------------------------------------------------------------------
testSV = do TestCase $ assertEqual "" 1.0 (sampleVariance pop1)

--------------------------------------------------------------------------------
-- Beispiel:
-- levenshtein (aa, ab)  = 1 
-- levenshtein (aa, bbb) = 3
-- levenshtein (ab, bbb) = 2
-- div = (1/6) * ( 1 + 3 + 2 ) = (1/6) * 6 = 1  
testDiv = do TestCase $ assertEqual "" 1.0 (diversity pop2)

--------------------------------------------------------------------------------
-- | Main
main = runTestTT $ TestList [ testAvg, testSV, testDiv ] 
