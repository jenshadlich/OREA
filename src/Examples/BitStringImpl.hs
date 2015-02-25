{-# language NoMonomorphismRestriction #-}
module Examples.BitStringImpl where

import Genetic.Types
import qualified Util.Random as R

--------------------------------------------------------------------------------
-- | Generate a random bit string of maxLength
generate :: Genpool Char -> Int -> IO (String)
generate gp maxLen = do
    r <- R.getRandomNumber(1, maxLen)
    i <- sequence $ do i <- [1 .. r]; return $ randomChar s
    return i
        where s = terminals gp

--------------------------------------------------------------------------------
-- | Evalutate: compare bit positions of indivuum and target
-- result is 0 if all positions are equal
evaluate :: Genpool Char -> String -> String -> Double
evaluate _ t i =
    fromInteger (toInteger (abs (length i - length t))) + 
    (sum $ map (\(x, y) -> if x == y then 0 else 1) (zip t i))

--------------------------------------------------------------------------------
-- | Crossover: Split and join at random positions
crossover :: String -> String -> IO ([String])
crossover m d = do 
    (mh, mt) <- splitRandom m
    (dh, dt) <- splitRandom d
    return $ [mh ++ dt, dh ++ mt]
    where
        splitRandom a = do
            r <- R.getRandomNumber(1, length a)
            return $ splitAt r a
            
--------------------------------------------------------------------------------
-- Mutation: insert a random terminal symbol at a random position
mutate :: Genpool Char -> String -> IO (String)
mutate gp i = do
    r <- R.getRandomNumber(0, length i)
    let (h, t) = splitAt r i
    c <- randomChar $ terminals gp
    return $ h ++ [c] ++ if length t > 0 then tail t else []

--------------------------------------------------------------------------------
-- Chose one random character
randomChar :: String -> IO (Char)
randomChar s = do
    r <- R.getRandomNumber (1, length s)
    return $ s !! (r-1)
