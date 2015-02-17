{-# language FlexibleContexts, ScopedTypeVariables #-}

module Regex.Trace where

import Genetic.Types
import Autolib.Exp.Type 
import Autolib.NFA
import qualified Autolib.NFA.Minus as M
import qualified Regex.Util as U
import qualified Util.Analysis as A
import Text.Printf

--------------------------------------------------------------------------------
-- | 
traceMissingWords :: (NFAC c Int, Floating f, PrintfArg f)
                  => Int -> (Population (NFA c Int) (RX c) (Genpool (RX c)) f) -> Int
                  -> IO ()
traceMissingWords generation pop n = do
    printf "%4d\tf_avg = %10.3f\tf_min = %8.1f\tmissing = "
        generation
        (A.averageFitness pop)
        (fitness $ head $ population pop)
    print $ take n $ accepted missing
    where
        indiv = genotype $ head $ population pop
        conf = config pop
        target = phenotype conf
        semantics = U.toNFA indiv
        missing = M.minus target semantics
        