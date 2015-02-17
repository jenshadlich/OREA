{-# language FlexibleContexts #-}

module UnitTests.Regex.Eval where

-- HUnit
import Test.HUnit

-- Dependancies
import Autolib.Exp.Inter
import Autolib.Exp.Type
import Genetic.Types

-- Module to test
import Regex.Eval

--------------------------------------------------------------------------------
setup s t i = (inter (std_sigma s) $ read t, read i)

--------------------------------------------------------------------------------
testFitness6f = do TestCase $ assertEqual "" 245.625 (evaluate6 t i)
    where 
        (t, i) = setup "a" "aaaaaa" "aaa" 

--------------------------------------------------------------------------------
testFitness6t = do TestCase $ assertEqual "" 11.0 (evaluate6 t i)
    where 
        (t, i) = setup "a" "aaaaaa" "aaaaaa" 

--------------------------------------------------------------------------------
testFitness5f = do TestCase $ assertEqual "" 20.625 (evaluate5 t i)
    where 
        (t, i) = setup "a" "aaaaaa" "aaa" 

--------------------------------------------------------------------------------
testFitness5t = do TestCase $ assertEqual "" 11.0 (evaluate5 t i)
    where 
        (t, i) = setup "a" "aaaaaa" "aaaaaa" 

--------------------------------------------------------------------------------
testFitness5fc = do TestCase $ assertEqual "" 10020.625 (evaluate5 t i)
    where 
        (t, i) = setup "a" "aaaaaa" "bbb" 

--------------------------------------------------------------------------------
-- | Main
main = runTestTT $ TestList [
    testFitness6f
    , testFitness6t 
    , testFitness5f
    , testFitness5t
    , testFitness5fc
    ] 
