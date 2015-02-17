{-# language FlexibleContexts #-}
-- | Enhaelt eine Sammlung von Funktionen fuer Baumoperationen auf RX-Datentyp.
-- Deklariert wird auch der Typ 'CRX' als Kontext von RX.
module Regex.Subtree where

import Autolib.NFA hiding (Dot)
import Autolib.Size
import Autolib.Exp.Type 
import Autolib.Exp.Syntax (subtrees)
import Autolib.Util.Zufall (eins)
import qualified Util.Random as R

--------------------------------------------------------------------------------
-- | Kontext von RX.
type CRX c = RX c -> RX c

--------------------------------------------------------------------------------
-- | Ueberprueft ob im regulaeren Ausdruck \"**\" (2x PowerStar) uebereinander
-- vorkommt; ja -> True, sonst False.
hasMultiStars :: NFAC c Int => RX c -> Bool
hasMultiStars rx = trav False rx
    where
    trav starFound rx' =
        case rx' of
            Letter    _   -> False
            Dot       a b -> (trav False a) || (trav False b) 
            Union     a b -> (trav False a) || (trav False b)
            PowerStar a   -> if starFound then True else trav True a

--------------------------------------------------------------------------------
-- | Zaehlt im regulaeren Ausdruck die Vorkommen von \"**\" (2x PowerStar)
-- uebereinander und liefert die Anzahl zurueck.
countMultiStars :: NFAC c Int => RX c -> Int
countMultiStars rx = trav False rx
    where
    trav starFound rx' =
        case rx' of
            Ref       _   -> 0
            Letter    _   -> 0
            Dot       a b -> (trav False a) + (trav False b) 
            Union     a b -> (trav False a) + (trav False b)
            PowerStar a   -> if starFound
                then 1 + trav False a  
                else     trav True a

--------------------------------------------------------------------------------
-- | Alle Teilbaeume eines regulaeren Ausdrucks inkl. Kontext berechnen und
-- als Liste zurueckgeben.
subtrees2 :: NFAC c Int => RX c -> [(CRX c, RX c)]
subtrees2 rx = trav root  ++ [root]
    where
    root = (id, rx) 
    trav :: (CRX c, RX c) -> [(CRX c, RX c)]
    trav (c, rx') =
        case rx' of
            Ref       _   -> []
            Letter    _   -> []
            Dot       a b -> binary Dot       a b 
            Union     a b -> binary Union     a b
            PowerStar a   -> unary  PowerStar a
            where
                unary op a =
                    let cx = (\s -> c (op s)) in
                    (trav (cx, a)) ++ [(cx, a)]
                binary op a b = 
                    let cxL = (\s -> c (op s b))
                        cxR = (\s -> c (op a s)) in
                    (trav (cxL, a)) ++ (trav (cxR, b)) ++ [(cxL, a), (cxR, b)]

--------------------------------------------------------------------------------
-- | Gleichvereilte zufaellige Auswahl eines Teilbaums mit Kontext.
randomSubtree2 :: NFAC c Int => RX c 
               -> IO (CRX c, RX c)
randomSubtree2 rx = do
    rst <- eins (subtrees2 rx)
    return $! rst

--------------------------------------------------------------------------------
-- | Gleichvereilte zufaellige Auswahl eines Teilbaums mit Kontext, wobei nur
-- Blaetter (= Symbole) beruecksichtigt werden.
randomLeaf :: NFAC c Int => RX c 
               -> IO (CRX c, RX c)
randomLeaf rx = do
    rst <- eins (filter (\(_,x) -> size x == 1) $ subtrees2 rx)
    return rst

--------------------------------------------------------------------------------
-- | Zufaelligen Teilbaum (ohne Kontex). Vewendet subtrees-Funktion aus der
-- Autolib.
randomSubtree :: NFAC c Int => RX c 
              -> IO (RX c)
randomSubtree t = do
    let st = subtrees t
    r <- R.getRandomNumber (0, (length (st)-1))
    let rst = st!!r
    return rst

--------------------------------------------------------------------------------
-- | Zufaellige binaere Operation (+, .) fuer RX
randomGlueOp :: IO (RX c -> RX c -> RX c)
randomGlueOp = do
    let ops = [Autolib.Exp.Type.Union, Autolib.Exp.Type.Dot]
    r <- R.getRandomNumber (0, (length (ops)-1))
    return $ ops!!r

--------------------------------------------------------------------------------
-- | Teilbaeume eines regulaeren Ausdrucks inkl. Kontext, 
-- nur homologe Bereiche (aehnliche Struktur) (vgl. A Field Guide to GP, S. 44).
homologousSubtrees :: NFAC c Int => RX c -> RX c
                -> [(CRX c, RX c)]
homologousSubtrees x y = trav (id, x, y) ++ [(id, x)]
    where
    trav :: (CRX c, RX c, RX c) -> [(CRX c, RX c)]
    trav (c, x', y') =
        if structure x' == structure y' -- Strukturvergleich
        then
            case x' of
                Ref       _   -> []
                Letter    _   -> []
                Dot       a b -> binary Dot       a b 
                Union     a b -> binary Union     a b
                PowerStar a   -> unary  PowerStar a
        else 
            []
        where
            unary op a =
                let cx = (\s -> c (op s)) in 
                (trav (cx, a, goDown y')) ++ [(cx, a)]
            binary op a b = 
                let cxL = (\s -> c (op s b))
                    cxR = (\s -> c (op a s)) in
                (trav (cxL, a, goLeft y')) ++ (trav (cxR, b, goRight y')) ++ [(cxL, a), (cxR, b)]
    -- nach links gehen
    goLeft :: RX c -> RX c
    goLeft rx = 
        case rx of
            Dot    a _ -> a 
            Union  a _ -> a
    -- nach rechts gehen
    goRight :: RX c -> RX c
    goRight rx = 
        case rx of
            Dot    _ b -> b 
            Union  _ b -> b
    -- nach unten gehen
    goDown :: RX c -> RX c
    goDown rx = 
        case rx of
            PowerStar a -> a 
    -- Struktur fuer Vergleich festlegen: gleicher Wert, bedeutet gleiche
    -- Struktur
    structure rx =
        case rx of
            Ref       _   -> 0
            Letter    _   -> 0
            Dot       _ _ -> 2 
            Union     _ _ -> 2
            PowerStar _   -> 1

--------------------------------------------------------------------------------
-- | Gleichvereilte zufaellige Auswahl eines homologen Teilbaums mit Kontext.
randomHomologousSubtree :: NFAC c Int => RX c -> RX c 
                        -> IO (CRX c, RX c)
randomHomologousSubtree x y = do
    rst <- eins (homologousSubtrees x y)
    return rst
