{-# language PatternGuards #-}
module Genetic.Selection.Parental where

import Genetic.Types
import qualified Util.Random as R
import qualified Genetic.Selection.RandomIndividuum as RI

import Data.List (sort)
import Control.Monad (forM)
import System.Random (Random, randomRIO)

--------------------------------------------------------------------------------
-- | Variation durch 'fitnessProp' und 'variateTournament'.
variateFitnessPropTournament :: (Show gt, Random f, Num f, Ord f)
                             => Population pt gt gp f -> Double -> Int
                             -> IO ([gt])
variateFitnessPropTournament pop prop tSize = do
    pop' <- fitnessProp pop
    variateTournament pop' prop tSize

--------------------------------------------------------------------------------
-- | Varation durch 'fitnessProp' und 'variateLinear'.
variateFitnessProp :: (Show gt, Random f, Num f, Ord f)
                   => Population pt gt gp f -> Double 
                   -> IO ([gt])
variateFitnessProp pop prop = do
    pop' <- fitnessProp pop
    variateLinear pop' prop

--------------------------------------------------------------------------------
-- | Fitnessproportionale Selektion (vgl. Weicker 2007, S. 71).
fitnessProp :: (Show gt, Random f, Num f, Ord f) =>
            Population pt gt gp f  
            -> IO (Population pt gt gp f)
fitnessProp pop = do
    is' <- sequence $ do [1 .. length is] ; return selectProportional
    return $ pop { population = sort is' }
    where
        is = population pop
        size = length is
        fs = (map (\i -> fitness i) is)
        rMax = foldl1 (+) fs
        sums = [foldl1 (+) $ take x fs | x <- [1 .. size]]
        selectProportional = do
            r <- System.Random.randomRIO (0, rMax)
            let idx = snd $  head $ filter (\(x, _) -> x > r) $ zip sums [0 .. size-1]
            return $ is !! idx

--------------------------------------------------------------------------------
-- | Turnierselektion.
variateTournament :: (Show gt, Ord f) =>
                  Population pt gt gp f -> Double -> Int 
                  -> IO ([gt])
variateTournament pop prop tSize = do
    cands <- variate is (length is)
    --cands <- variate is (min 50 (length is))
    return $ cands
    where
        is = population pop
        conf = config pop
        variate is' n
            | n >= 1 = do
                r <- R.getRandomNumber (1, 100)
                if r <= (round (prop * 100))
                    then do
                        a <- tournament is tSize
                        b <- tournament is tSize
                        c <- crossover conf (genotype a) (genotype b)
                        next <- variate is' (n-1)
                        return $ next ++ c
                    else do
                        a <- tournament is tSize
                        c <- mutate conf $ genotype a
                        next <- variate is' (n-1)
                        return $ next ++ [c]
            | otherwise = return []

--------------------------------------------------------------------------------
-- | Ein einzelnes Turnier ausfuehren. Rueckgabewert ist der Turniersieger.
tournament :: (Ord f) =>
           [Individuum gt f] -> Int
           -> IO (Individuum gt f)
tournament is n 
    | n > 0 = do
        i <- RI.randomIndividuum is
        c <- tournament is (n-1)
        return $ min i c
    | otherwise = RI.randomIndividuum is

--------------------------------------------------------------------------------
-- | Lineare Variation. Entsprechend der Wahrscheinlichkeit wird fuer die
-- Population eine lineare Rekombination oder Mutation ausgefuehrt.
variateLinear :: Population pt gt gp f -> Double
              -> IO ([gt])
variateLinear pop prop = do
    r <- R.getRandomNumber (1, 100)
    if r <= (round (prop * 100))
        then (recombinateLinear pop)
        else (mutateLinear pop)

--------------------------------------------------------------------------------
-- | Lineare Rekombination. Iteration der Liste und paarweise Rekombination.
recombinateLinear :: Population pt gt gp f
                  -> IO ([gt])
recombinateLinear pop = do
    let is = map genotype $ population pop
    cs <- pairs is (length is)
    return $ cs
    where
        pairs ps n
            | n >= 2 = do
                let a = ps !! (n-1)
                let b = ps !! (n-2)
                c <- crossover (config pop) a b
                r <- pairs ps (n-2)
                return $ r ++ c
            | otherwise =  return []

--------------------------------------------------------------------------------
-- | Alle Individuen werden mutiert.
mutateLinear :: Population pt gt gp f
             -> IO ([gt])
mutateLinear pop = do
    let is = map genotype $ population pop
    forM is $ \i -> mutate (config pop) i

--------------------------------------------------------------------------------
-- | Mutationsfunktion ensprechend Konfiguration aufrufen.
mutate :: (Configuration pt gt gp f) -> gt
       -> IO (gt)
mutate conf i = do
    m <- fMutate conf (genpool $ conf) i
    return m

--------------------------------------------------------------------------------
-- | Rekombinationsfunktion ensprechend Konfiguration aufrufen.
crossover :: (Configuration pt gt gp f) -> gt -> gt -> IO ([gt])
crossover conf a b = do
    c <- fCrossover conf a b
    return c
