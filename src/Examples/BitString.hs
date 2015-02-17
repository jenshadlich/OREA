module Example.BitString where

import Genetic.Types
import Genetic.Central
import Genetic.Trace
import qualified Examples.BitStringImpl as BS
import qualified Util.Time
   
--------------------------------------------------------------------------------
-- | Beispielprogramm um einen Bitstring evolutionaer zu berechnen.
-- Dabei werden nur Terminale 0 und 1 als Buchstaben verwendet (keine Funktionen
-- im Genpool).
main :: IO()
main = do
    let config = Configuration {
        maxGenerations    = 100,
        maxPopulationSize = 50,
        threshold         = Just 0,
        phenotype         = target,
        genpool           = Genpool { terminals = "01", functions = [] },
        seed              = Nothing,
        parSelection      = Tournament 0.8 2,
        envSelection      = Best100,
        fDecode           = id,
        fFitness          = BS.evaluate,
        fTracePop         = tracePrint, -- komplette Population ausgeben
        fGenerate         = (\s -> BS.generate s $ (length target) * 2),
        fMutate           = BS.mutate,
        fCrossover        = BS.crossover
    } where
        target = "111100001111000011110000" :: String

    (_, result) <- Util.Time.stopTime (evolve config)
    showWinner result