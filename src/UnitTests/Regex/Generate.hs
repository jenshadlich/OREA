{-# language FlexibleContexts #-}

module UnitTests.Regex.Generate where

import UnitTests.Assert

-- Dependancies
import Autolib.Exp.Inter
import Autolib.Exp.Type
import Autolib.NFA hiding (Dot)
import Genetic.Types

-- Module to test
import Regex.Generate
import Regex.Util

--------------------------------------------------------------------------------
simpleGenpool :: (NFAC c Int) => [c] -> Genpool (RX c)
simpleGenpool s = Genpool {
        terminals = map Letter s,
        functions = map Fun1 [PowerStar] ++ map Fun2 [Union, Dot] 
    }

--------------------------------------------------------------------------------
testGen ds = do
    rxs <- mapM (\d -> generateRX (simpleGenpool "ab") d) ds
    mapM (\(d,x) -> assertLq "" d (depth x)) (zip ds rxs)
    
--------------------------------------------------------------------------------
-- | Main
main = do
    testGen [2, 3 .. 9]
    return ()