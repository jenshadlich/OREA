{-# language PatternGuards #-}
module Genetic.Central (evolve) where

import Genetic.Types
import qualified Genetic.Selection.Parental as P
import qualified Genetic.Selection.Environmental as E
import List (sort)
import System.Random (Random)
import qualified Util.Parallel as Parallel

{-
    evolve:
    ----------------
    | initialize   |  Initialisierung (Zufall) und initiale Bewertung
    |--------------|
    | evoLoop      |  solange Abbruchkriterium (Anzahl Generationen oder
    |              |  Schwellwert unterschritten) nicht erfuellt
    |   -----------|
    |   || step   ||  Evolutionsschritt
    ----------------

    step:
    ---------------|
    | variate      |  Variation:
    |              |  - Elternselektion
    |              |  - Rekombination und / oder Mutation 
    |--------------|
    | evaluate     |  Bewertung der neuen Individuen
    |--------------|
    | select       |  Selektion der Bestangespassten (Umweltselektion)
    ----------------
-}

--------------------------------------------------------------------------------
-- | Die Funktion 'evolve' fuehrt den evolutionaeren Algorithmus entsprechend
-- der uebergebenen Konfiguration aus. Dabei wird zuerst die Population
-- initialisiert. Solange das Abbruchkriterium (Anzahl Generationen oder
-- Schwellwert unterschritten) nicht erfuellt ist, wird jeweils ein
-- Evolutionsschritt (Variation, Bewertung, Selektion) ausgefuehrt.
evolve :: (Eq gt, Show gt, Random f, Num f, Eq f, Ord f)
       => Configuration pt gt gp f 
       -> IO (Population pt gt gp f)
evolve conf = do 
    initPop <- initialize conf
    evoLoop (maxGenerations conf) initPop
    where
        evoLoop n pop 
            | (n > 0) && (continue pop) = do
                trace n pop
                next <- step pop
                evoLoop (n-1) next
            | otherwise = do
                trace n pop
                return pop
        trace n pop = (fTracePop conf) ((maxGenerations conf) - n) pop
        continue pop = case (threshold conf) of
            Nothing -> True
            Just a  -> bestFitness pop > a

--------------------------------------------------------------------------------
-- | initialize
initialize :: (Show gt, Ord f) => (Configuration pt gt gp f) 
           -> IO (Population pt gt gp f)
initialize conf = do
    is <- sequence $ do [1 .. size]; return $ (fGenerate conf) (genpool conf)
    return $ Population {
        config = conf,
        population = sort $ evaluate conf (seeds ++ is)
    }
    where
        seeds = case seed (conf) of
            Nothing -> []
            Just a  -> a
        size = (maxPopulationSize conf)

--------------------------------------------------------------------------------
-- | step
step :: (Eq gt, Show gt, Random f, Num f, Ord f)
     => Population pt gt gp f 
     -> IO (Population pt gt gp f)
step pop = do
    cands <- variate pop
    let cands' = evaluate (config pop) cands
    let pop' = pop { population = cands' ++ population pop }
    select pop' 

--------------------------------------------------------------------------------
-- | variate
-- Variation mit Elternselektion
variate :: (Show gt, Random f, Num f, Ord f) => Population pt gt gp f
        -> IO ([gt])
variate pop | Linear prop <- parSelection (config pop)
            = P.variateLinear pop prop
variate pop | Tournament prop tSize <- parSelection (config pop)
            = P.variateTournament pop prop tSize
variate pop | FitnessProp prop <- parSelection (config pop)
            = P.variateFitnessProp pop prop
variate pop | FitnessPropWithTournament prop tSize <- parSelection (config pop)
            = P.variateFitnessPropTournament pop prop tSize

--------------------------------------------------------------------------------
-- | evaluate
evaluate :: (Configuration tt t c f) -> [t]
         -> [Individuum t f]
evaluate conf is = Parallel.parallelMap (\i -> mkIndividuum conf i) is

--------------------------------------------------------------------------------
-- | Umweltselektion
select :: (Eq gt, Ord f) => Population pt gt gp f 
       -> IO (Population pt gt gp f)
select pop | Best100 <- envSelection (config pop)
           = E.best100 pop
select pop | Best100Unique <- envSelection (config pop)
           = E.best100Unique pop
select pop | Best80Random20 <- envSelection (config pop)
           = E.best80random20 pop
select pop | NAryDoubleTournament tSize <- envSelection (config pop)
           = E.nAryDoubleTournament pop tSize

--------------------------------------------------------------------------------
-- Hilfsfunktionen
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
bestFitness :: (Ord f) => Population pt gt gp f -> f
bestFitness pop = fitness $ head $ sort $ population pop

--------------------------------------------------------------------------------
-- | mkIndividuum
mkIndividuum :: (Configuration pt gt gp f) -> gt -> Individuum gt f 
mkIndividuum conf i = Individuum {
        genotype = i,
        fitness = (fFitness conf) (genpool conf) (phenotype conf) i
    }
