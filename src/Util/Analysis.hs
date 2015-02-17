
module Util.Analysis where

import Genetic.Types
import Util.Levenshtein

--------------------------------------------------------------------------------
-- | Berechnung der empirischen Varianz der Fitness einer Population.
sampleVariance :: (Fractional f) => Population pt gt gp f -> f
sampleVariance pop =
    1 / (n-1) * (sum (map square xs) - ((1/n) * (square (sum xs)))) 
    where
        square = (\x -> x*x)
        n = fromIntegral (length is)
        xs = map (\x -> fitness x) is
        is = population pop

--------------------------------------------------------------------------------
-- | Berechnung der durchschnittliche Fitness einer Population.
averageFitness :: (Fractional f)
               => Population pt gt gp f
               -> f
averageFitness pop = 
    (sum $ map (\i -> fitness i) (population pop)) / 
    fromIntegral (length (population pop))

--------------------------------------------------------------------------------
-- | Berechnung der Diversitaet einer Population unter Verwendung des
-- Editierabstands.
diversity :: (Show gt)
          => Population pt gt gp f
          -> Double
diversity pop = (fromIntegral su) * (1 / (s * (s - 1)))
    where
        is = population pop
        l = length is
        s = fromIntegral l
        su = sum $ [ distance n m | n <- [1 .. l], m <- [1 .. l], m > n]
        distance = \n m -> levenshtein (idx n) (idx m)
        idx i = show $ genotype $ is !! (i-1)
