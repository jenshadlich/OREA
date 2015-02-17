{-# language FlexibleContexts #-}
module Regex.Ops.Mutation where

import Autolib.NFA hiding (Dot)
import Autolib.Exp.Type
import Autolib.Util.Zufall (someIO, eins)
import Genetic.Types

import qualified Regex.Subtree as S
import qualified Regex.Generate as G
import qualified Util.Random as R
import qualified Regex.Util as U

--------------------------------------------------------------------------------
-- | Verschiedene Mutationen, zufaellig (gleichverteilt) ausgewaehlt.
-- Ruft 'hoist', 'partCollapse', 'subtreeWord' und 'subtreeN'.
mutate :: NFAC c Int => Genpool (RX c) -> RX c
       -> IO (RX c)
mutate gp i = do
    let muts = [hoist, partCollapse, subtreeWord, subtreeN 3]
    m <- eins muts
    m gp i

--------------------------------------------------------------------------------
-- | Verschiedene Mutationen, zufaellig (gleichverteilt) ausgewaehlt
-- Ruft 'hoist', 'partCollapse' und 'subtreeN'.
mutate2 :: NFAC c Int => Genpool (RX c) -> RX c
       -> IO (RX c)
mutate2 gp i = do
    let muts = [hoist, partCollapse, subtreeN 3]
    m <- eins muts
    m gp i

--------------------------------------------------------------------------------
-- | Verschiedene Mutationen, zufaellig (gleichverteilt) ausgewaehlt
-- Ruft 'hoist', 'collapse', 'partCollapse', 'subtreeN', 'expandWordPlus' und
-- 'powerStar'.
mutate3 :: NFAC c Int => Genpool (RX c) -> RX c
        -> IO (RX c)
mutate3 gp i = do
    let muts = [
            hoist
            , collapse
            , duplication
            , partCollapse
            , subtreeN 3
            , expandWordPlus
            , powerStar
            ]
    m <- eins muts
    m gp i

--------------------------------------------------------------------------------
-- | Kleenesche Huelle (Stern) auf einen Teilbaum.
powerStar :: NFAC c Int => Genpool (RX c) -> RX c
          -> IO (RX c)
powerStar _ i = do 
    ( c, i' ) <- S.randomSubtree2 i
    return $ c (PowerStar i') 

--------------------------------------------------------------------------------
-- | Sonderform von collapse: ein Teilbaum wird durch einen Teilbaum seinerselbst
-- ersetzt.
partCollapse :: NFAC c Int => Genpool (RX c) -> RX c
             -> IO (RX c)
partCollapse _ i = do
    ( c, j ) <- S.randomSubtree2 i
    ( _, k ) <- S.randomSubtree2 j
    return $ c k

--------------------------------------------------------------------------------
-- | Teilbaum wird hochgezogen und ersetzt bisherigen Baum (neue Wurzel).
hoist :: NFAC c Int => Genpool (RX c) -> RX c
      -> IO (RX c)
hoist _ i = do
    (_, i') <- S.randomSubtree2 i
    return i'

--------------------------------------------------------------------------------
--  | Ein Teilbaum wird durch ein zufaelliges Terminalsymbol (= Letter) ersetzt.
collapse :: NFAC c Int => Genpool (RX c) -> RX c
         -> IO (RX c)
collapse gp i = do
    (c, _) <- S.randomSubtree2 i
    e <- eins $ terminals gp
    return $ c e

--------------------------------------------------------------------------------
-- | Ein Teilbaum wird durch einen zufaellig generierten Baum ersetzt.
subtreeN :: NFAC c Int => Int -> Genpool (RX c) -> RX c
         -> IO (RX c)
subtreeN n gp i = do
    (c, _) <- S.randomSubtree2 i
    i' <- G.generateRX gp n
    return $ c i' 

--------------------------------------------------------------------------------
-- | Sonderform von subtree: Teilbaum wird durch ein zufaelliges Wort ersetzt.
subtreeWord :: NFAC c Int => Genpool (RX c) -> RX c
            -> IO (RX c)
subtreeWord gp i = do
    (c, _) <- S.randomSubtree2 i
    l <- R.getRandomNumber (2, 5)
    w <- someIO (U.sigmaFromGenpool gp) l
    return $ c (foldr1 Dot $ map Letter w) 

--------------------------------------------------------------------------------
-- | Ein Blatt wird durch einen zufaellig generierten Baum ersetzt.
expandN :: NFAC c Int => Int -> Genpool (RX c) -> RX c
         -> IO (RX c)
expandN n gp i = do
    (c, _) <- S.randomLeaf i
    i' <- G.generateRX gp n
    return $ c i' 

--------------------------------------------------------------------------------
-- | Sonderform von expand: 
-- Terminal wird durch ein ein Teilbaum bestehend aus
-- Terminal + (zufaelliges Wort) ersetzt.
expandWordPlus :: NFAC c Int => Genpool (RX c) -> RX c
               -> IO (RX c)
expandWordPlus gp i = do
    (c, i') <- S.randomLeaf i
    l <- R.getRandomNumber (2, 5)
    w <- someIO (U.sigmaFromGenpool gp) l
    return $ c (Union i' (foldr1 Dot $ map Letter w)) 

--------------------------------------------------------------------------------
-- | Duplication: 
-- Terminal wird durch einen Teilbaum desselben Baumes ersetzt.
duplication :: NFAC c Int => Genpool (RX c) -> RX c
            -> IO (RX c)
duplication _ i = do
    (c, _) <- S.randomLeaf i
    (_, k) <- S.randomSubtree2 i
    return $ c k

--------------------------------------------------------------------------------
-- | Point mutation: 
-- Terminal wird durch ein zufaelliges Terminal ersetzt.
-- Dot wird zu Union und umgekehrt
-- PowerStar bleibt PowerStar (da keine weitere einstellige Op. zur
-- Verfuegung steht)
point :: NFAC c Int => Genpool (RX c) -> RX c
      -> IO (RX c)
point gp i = do
    (c, i') <- S.randomSubtree2 i
    x <- do 
        case i' of
            Letter    _   -> do l <- eins $ terminals $ gp; return l 
            Dot       a b -> return $ Union a b 
            Union     a b -> return $ Dot   a b
            PowerStar a   -> return $ PowerStar a
    return $ c x