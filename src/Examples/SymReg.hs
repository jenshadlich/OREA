module Examples.SymReg where

import Genetic.Types
import Genetic.Central
import Genetic.Trace
import qualified Examples.SymRegImpl as I
import Examples.SymRegTypes
import qualified Util.Time

--------------------------------------------------------------------------------
-- | Genetic programming in action: symbolic regression of sin(x) ([-pi, pi], 21 sampling points)
main :: IO()
main = do
    -- genpool
    let gp = Genpool {
        terminals = [VarX],
        --functions = map Fun1 [Sqr, Sqrt] ++ map Fun2 [Add, Sub, Mul, Div]
        functions = map Fun2 [Add, Sub, Mul] -- limit available functions
    }
    -- do the config
    let config = Configuration {
        maxGenerations    = 500,
        maxPopulationSize = 100,
        threshold         = Nothing,
        phenotype         = decode target,
        genpool           = gp,
        seed              = Nothing,
        parSelection      = Tournament 0.8 4,
        envSelection      = Best100Unique,
        fDecode           = decode,
        fFitness          = I.evaluate xrange,
        fTracePop         = tracePrintStats,
        fGenerate         = (\gp -> I.generate crange gp 3),
        fMutate           = I.mutate crange,
        fCrossover        = I.crossover
    } where
        decode = (\t -> (map (\x -> I.eval x t)) xrange) -- compute the function value
        target = (Sin VarX) -- target function = sin(x)
        xrange = map (\x -> x - pi) (map (\x -> (x/samples) * 2 * pi) [0, 1 .. samples])
        samples = 20
        crange = [0.05, 0.10 .. 1.0]

    (_, result) <- Util.Time.stopTime (evolve config)
    showWinner result
