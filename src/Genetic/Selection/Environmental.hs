{-# language PatternGuards #-}
module Genetic.Selection.Environmental where

import Genetic.Types
import qualified Util.Random as R
import qualified Genetic.Selection.RandomIndividuum as RI

import System.Random
import Data.List (sort, sortBy, nubBy)
import Control.Monad (forM)

--------------------------------------------------------------------------------
-- | "best" selection strategy, 100%
best100 :: (Ord f) => Population pt gt gp f 
        -> IO (Population pt gt gp f)
best100 pop = return $
    pop { population = take size $ sort p }
    where
        p = population pop
        size = maxPopulationSize (config pop)

--------------------------------------------------------------------------------
-- | modified "best" selection strategy, 100%, w/o duplicates
best100Unique :: (Eq gt, Ord f) => Population pt gt gp f 
              -> IO (Population pt gt gp f)
best100Unique pop = return $
    pop { population = take size $ makeUnique $ sort p }
    where
        p = population pop
        size = maxPopulationSize (config pop)

--------------------------------------------------------------------------------
-- | modified "best" selection strategy, 80% best, 20% random
-- (experimental)
best80random20 :: (Ord f) => Population pt gt gp f 
               -> IO (Population pt gt gp f)
best80random20 pop = do
    random20' <- random20
    return pop { population = best80 ++ random20' }
    where
        best80 = take size80 $ sort p
        p = population pop
        size = maxPopulationSize (config pop)
        size80 = truncate (0.8 * toRational (size))
        size20 = size - size80
        random20 = sequence $ do [1 .. size20] ; return randomI
        randomI = do
            r <- R.getRandomNumber (1, length rest)
            return $ rest !! (r-1)
            where
                rest = snd $ splitAt size80 (sort p) 

--------------------------------------------------------------------------------
-- | Modified q-stage double tournament selection (see Weicker 2007, p 69).
-- uses unique genotypes and a random offset for the number of victories (double counting of victories)
nAryDoubleTournament :: (Eq gt, Ord f) => Population pt gt gp f -> Int
                     -> IO (Population pt gt gp f)
nAryDoubleTournament pop n = do
    is' <- forM is runTournament
    return $ pop {
        population = take size 
                   $ sort 
                   $ map (\x -> snd x ) 
                   $ sortByVictories is'
    }
    where
        is = makeUnique $ population pop
        size = maxPopulationSize (config pop)
        sortByVictories = sortBy (\(a, _) (b, _) -> if a > b then LT else GT)
        runTournament i = do
            victories <- sequence $ do [1 .. n] ; return $ fight i
            return (sum victories, i)
        fight i = do
            contender <- RI.randomIndividuum is
            let res = if i <= contender then 1 else 0
            offset <- randomRIO ( 0, 1 :: Double )
            return $ res + offset

--------------------------------------------------------------------------------
-- | Eliminate multiple occurances of the same genotype
makeUnique :: (Eq gt) => [Individuum gt f]
           -> [Individuum gt f] 
makeUnique = nubBy (\x y -> genotype x == genotype y )
