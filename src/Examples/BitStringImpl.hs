{-# language NoMonomorphismRestriction #-}
module Examples.BitStringImpl where

import Genetic.Types
import qualified Util.Random as R

--------------------------------------------------------------------------------
-- | Zufaelligen Bitstring mit einer max. Laenge erzeugen.
generate :: Genpool Char -> Int -> IO (String)
generate gp maxLen = do
    r <- R.getRandomNumber(1, maxLen)
    i <- sequence $ do i <- [1 .. r]; return $ randomChar s
    return i
        where s = terminals gp

--------------------------------------------------------------------------------
-- | Bewertung: Vergleich der Bitpositionen von Individuum und Ziel-Bitstring.
-- Ist 0 wenn alle Positionen und die Laenge uebereinstimmen.
evaluate :: Genpool Char -> String -> String -> Double
evaluate _ t i =
    fromInteger (toInteger (abs (length i - length t))) + 
    (sum $ map (\(x, y) -> if x == y then 0 else 1) (zip t i))

--------------------------------------------------------------------------------
-- | Rekombination: 2 Bitstrings an zufaellingen Positionen auftrennen und
-- rekombinieren.
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
-- Mutation: Fuegt ein zufaelliges Terminal an zufaelliger Position ein. 
mutate :: Genpool Char -> String -> IO (String)
mutate gp i = do
    r <- R.getRandomNumber(0, length i)
    let (h, t) = splitAt r i
    c <- randomChar $ terminals gp
    return $ h ++ [c] ++ if length t > 0 then tail t else []

--------------------------------------------------------------------------------
-- Zufaelliges Terminal auswaehlen.
randomChar :: String -> IO (Char)
randomChar s = do
    r <- R.getRandomNumber (1, length s)
    return $ s !! (r-1)
