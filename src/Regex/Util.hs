{-# language FlexibleContexts #-}
module Regex.Util (toNFA, toNFA', mkRX, serialize, depth, sigmaFromGenpool) where

import Autolib.Exp.Type
import Autolib.Exp.Inter
import Autolib.NFA hiding (Dot)
import Genetic.Types

--------------------------------------------------------------------------------
-- | Konstruktion eines aequivalenten NFA aus einem RX.
toNFA :: NFAC c Int => RX c -> NFA c Int
toNFA rx =
    {-# SCC "toNFA" #-}
    inter (std_sigma []) rx

--------------------------------------------------------------------------------
-- | Konstruktion eines aequivalenten NFA aus einem RX.
toNFA' :: NFAC c Int => [c] -> RX c -> NFA c Int
toNFA' s rx =
    {-# SCC "toNFA2" #-}
    inter (std_sigma s) rx

--------------------------------------------------------------------------------
-- | Regularen Ausdruck aus String parsen und Alphabet und RX erzeugen.
mkRX :: NFAC c Int => [c] -> String -> ([c], RX c)
mkRX sigma regex = (sigma, read regex)

--------------------------------------------------------------------------------
-- | RX wird traversiert und als String zurueckgegeben, sodass beim erneuten
-- Parsen (read) die Struktur des regulaeren Ausdrucks erhalten bleibt
-- (im Gegensatz zu show).
serialize :: (NFAC c Int) => RX c -> String
serialize rx = trav rx
    where
    trav rx' =
        case rx' of
            Ref       r   -> "(" ++ r ++ ")"
            Letter    l   -> "(" ++ mkLetter l ++ ")"
            Dot       a b -> "(" ++ (trav a) ++ " " ++ (trav b) ++ ")" 
            Union     a b -> "(" ++ (trav a) ++ "+" ++ (trav b) ++ ")"
            PowerStar a   -> "(" ++ (trav a) ++ ")^*"
    mkLetter l = (filter (\x -> x /= '\'') $ show l)

--------------------------------------------------------------------------------
-- | Tiefe des Syntaxbaumes eines RX berechnen.
depth :: (NFAC c Int) => RX c -> Int
depth rx = trav rx
    where
    trav rx' =
        case rx' of
            Letter    _   -> 1
            Dot       a b -> 1 + max (trav a) (trav b)
            Union     a b -> 1 + max (trav a) (trav b)
            PowerStar a   -> 1 + (trav a)

--------------------------------------------------------------------------------
-- | Extrahiert Alphabet aus Genpool.
sigmaFromGenpool :: (NFAC c Int) => Genpool (RX c) -> [c]
sigmaFromGenpool gp = 
    map (\x -> case x of Letter a -> a) $ filter isLetter $ terminals gp
    where
        isLetter x = case x of Letter _ -> True; otherwise -> False
