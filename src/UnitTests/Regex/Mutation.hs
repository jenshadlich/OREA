{-# language FlexibleContexts #-}

module UnitTests.Regex.Mutation where

import UnitTests.Assert

-- Dependancies
import Autolib.Exp.Type
import Autolib.NFA hiding (Dot)
import Genetic.Types
import Regex.Util

-- Module to test
import Regex.Ops.Mutation

--------------------------------------------------------------------------------
simpleGenpool :: (NFAC c Int) => [c] -> Genpool (RX c)
simpleGenpool s = Genpool {
        terminals = map Letter s,
        functions = map Fun1 [PowerStar] ++ map Fun2 [Union, Dot] 
    }

--------------------------------------------------------------------------------
testPoint = do
    mapM_ (\_ -> runTest) [1..100] 
    where
        runTest = do
            m <- point (simpleGenpool "ab") rx
            assertTrue (show rx ++ " : " ++ show m) (elem m rs)
        (_, rx) = mkRX "ab" "(aa + b)^*"
        e = ["(aa + b)^*",    "(a(ab))^*",    "(aa + a)^*", "((aa)b)^*",
             "(a + a + b)^*", "(ba + b)^*", "(ab + b)^*"]
        rs = map (\x -> snd $ mkRX "ab" x) e

--------------------------------------------------------------------------------
testAll = do
    mapM_ (\_ -> runTest) [1..100] 
    where
        runTest = do
            m <- mutate (simpleGenpool "ab") rx
            print $ serialize  m
            m <- mutate2 (simpleGenpool "ab") rx
            print $ serialize  m
            m <- mutate3 (simpleGenpool "ab") rx
            print $ serialize  m
            assertTrue "" True
        (_, rx) = mkRX "ab" "(aa + b)^*"

--------------------------------------------------------------------------------
-- | Main
main = do
    testPoint
    testAll
    return ()