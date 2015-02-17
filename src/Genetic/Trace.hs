{-# language FlexibleContexts, ScopedTypeVariables #-}

module Genetic.Trace where

import Genetic.Types
import qualified Util.Analysis as A
import Text.Printf

--------------------------------------------------------------------------------
-- | keine Ausgaben
noTrace :: Int -> Population pt gt gp f
        -> IO ()
noTrace _ _ = return ()

--------------------------------------------------------------------------------
-- | aller 10 Generationen die aktuelle Generation ausgeben
tracePrintGen10 :: Int -> Population pt gt gp f
                -> IO ()
tracePrintGen10 n _ = 
    if (mod n 10) == 0 
        then do printf "%4d\n" n; return ()
        else return ()

--------------------------------------------------------------------------------
-- | gibt die komplette Population aus
tracePrint :: (Show gt, Show f)
           => Int -> Population pt gt gp f
           -> IO ()
tracePrint = (\_ pop -> mapM_ print $ population pop)

--------------------------------------------------------------------------------
-- | Ausgabe von: durchschnittlicher Fitness, bester Fitness und
-- Standardabweichung der Fitness
tracePrintStats :: (Floating f, PrintfArg f)
                => Int -> Population pt gt gp f
                -> IO ()
tracePrintStats n pop = 
    printf "%4d\tf_avg = %10.3f\tf_min = %8.1f\ts = %8.1f\n"
        n
        (A.averageFitness pop)
        (fitness $ head $ population pop)
        (sqrt $ A.sampleVariance pop)

--------------------------------------------------------------------------------
-- | Ausgabe wie 'tracePrintStats' und zusaetzlich die Diversitaet
-- (Achtung: Berechnung der Diversitaet sehr rechenintensiv) 
tracePrintStatsWDiv :: (Show gt, Floating f, PrintfArg f)
                    => Int -> Population pt gt gp f
                    -> IO ()
tracePrintStatsWDiv n pop = 
    printf "%4d\tf_avg = %10.3f\tf_min = %8.1f\ts = %8.1f\tdiv = %10.1f\n"
        n 
        (A.averageFitness pop)
        (fitness $ head $ population pop)
        (sqrt $ A.sampleVariance pop)
        (A.diversity pop)

--------------------------------------------------------------------------------
-- | gibt das erste (= das beste; wegen aufsteigender Sortierung) Indivuduum
-- als Sieger aus
showWinner :: (Show gt, Show f)
           => Population pt gt gp f
           -> IO ()
showWinner pop = do
    putStrLn "__________________"
    putStrLn "And the winner is:"
    print $ head $ population pop
