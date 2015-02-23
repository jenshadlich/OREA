
module Util.Time (stopTime) where

import Text.Printf
import System.CPUTime

--------------------------------------------------------------------------------
-- | Executes the given function and returns the execution time and the function's result
stopTime :: IO t -> IO (Double, t)
stopTime exec = do
    start <- getCPUTime
    result <- exec
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (1000000000000 :: Double)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return (diff, result)