
module Util.Levenshtein where

import Data.Array

--------------------------------------------------------------------------------
-- | Compute Levenshtein distance
levenshtein :: (Eq a) => [a] -> [a] -> Int
levenshtein s t = 
    -- d !! (length s) !! (length t) 
    d ! (length s, length t)
    where
        -- d = [[distance m n | n <- [0 .. length t]] | m <- [0 .. length s]]
        bnd = ((0,0),(length s,length t)) 
        d = array bnd $ do (i,j) <- range bnd ; return ( (i,j), distance i j)
        distance i 0 = i
        distance 0 j = j
        distance i j = minimum [a, b, c]
            where
                a = d ! (i-1 , j) + 1
                b = d ! (i, j-1) + 1
                c = d ! (i-1, j-1) + cost
                cost = if s !! (i-1) == t !! (j-1) then 0 else 1
