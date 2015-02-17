{-# language FlexibleContexts #-}

module UnitTests.Genetic.Helper where

import Genetic.Central
import Genetic.Trace
import Genetic.Types

--------------------------------------------------------------------------------
setup parSel envSel threshold' = 
    Configuration {
        maxGenerations    = 10,
        maxPopulationSize = 10,
        threshold         = threshold',
        phenotype         = "",
        genpool           = Genpool { terminals = "01", functions = [] },
        seed              = Nothing,
        parSelection      = parSel,      
        envSelection      = envSel,
        fDecode           = id,
        fFitness          = (\_ _ i -> (fromIntegral (length i)) :: Double),
        fTracePop         = (\n pop -> do 
            let p = population pop
            return ()),
        fGenerate         = (\gp -> do return $ terminals gp),
        fMutate           = (\gp x -> do return $ x ++ terminals gp),
        fCrossover        = (\x y -> do return [x ++ y, y ++ x])
    }

--------------------------------------------------------------------------------
runEvolve parSel envSel threshold' = do
    result <- evolve conf
    return result
    where 
        conf = setup parSel envSel threshold' 
