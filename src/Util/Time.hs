
module Util.Time (stopTime) where

import Text.Printf
import System.CPUTime

--------------------------------------------------------------------------------
-- | Fuehrt die uebergebene Funktion aus und stoppt die CPU-Zeit deren
-- Ausfuehrung.
stopTime :: IO t -> IO (Double, t)
stopTime exec = do
    start <- getCPUTime
    result <- exec
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (1000000000000 :: Double)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return (diff, result)