{-# language FlexibleContexts #-}
module Regex.Ops.Crossover where

import Autolib.NFA hiding (Dot)
import Autolib.Exp.Type
import Autolib.Util.Zufall (eins)

import qualified Util.Random as R
import qualified Regex.Subtree as S

--------------------------------------------------------------------------------
-- | Baumtausch-Rekombination (2 verschiedene Implementierungen).
-- Ruft zufaellig 'randomTreeCrossover' oder 'randomTreeCrossover2'.
recombinate :: NFAC c Int => RX c -> RX c 
            -> IO [ RX c ]
recombinate a b = do
    let recs = [randomTreeCrossover, randomTreeCrossover2]
    r <- eins recs
    r a b

--------------------------------------------------------------------------------
-- | Verschiedene Rekombinationen, zufaellig ausgewaehlt.
-- Ruft 'randomTreeCrossover', 'randomTreeCrossover2', Vereinigung oder
-- Verketten.
recombinate2 :: NFAC c Int => RX c -> RX c 
             -> IO [ RX c ]
recombinate2 a b = do
    let recs = [
            randomTreeCrossover
            , randomTreeCrossover2
            , op Autolib.Exp.Type.Dot
            , op Autolib.Exp.Type.Union 
            ]
    r <- R.getRandomNumber (1, length recs)
    (recs !! (r-1)) a b

--------------------------------------------------------------------------------
-- | Verschiedene Rekombinationen, zufaellig ausgewaehlt.
-- Ruft 'homologousTreeCrossover', 'randomTreeCrossover2', Vereinigung oder
-- Verketten.
recombinate3 :: NFAC c Int => RX c -> RX c 
             -> IO [ RX c ]
recombinate3 a b = do
    let recs = [
            randomTreeCrossover2
            , homologousTreeCrossover
            , op Autolib.Exp.Type.Dot
            , op Autolib.Exp.Type.Union 
            ]
    r <- eins recs
    r a b

--------------------------------------------------------------------------------
op :: (RX c -> RX c -> RX c) -> RX c -> RX c -> IO [RX c]
op o mom dad = return [ o mom dad ]

--------------------------------------------------------------------------------
-- | Baumtausch-Rekombination. Verwendet subtrees-Funktion aus Autlib.
randomTreeCrossover :: NFAC c Int => RX c -> RX c 
                    -> IO [ RX c ]
randomTreeCrossover mom dad = do
	m <- S.randomSubtree mom
	d <- S.randomSubtree dad
	rop <- S.randomGlueOp
	return $ [rop m d] ++ [rop d m]

--------------------------------------------------------------------------------
-- | Baumtausch-Rekombination. Verwendet Kontexte.
randomTreeCrossover2 :: NFAC c Int => RX c -> RX c 
                    -> IO [ RX c ]
randomTreeCrossover2 mom dad = do
    (cm, m) <- S.randomSubtree2 mom
    (cd, d) <- S.randomSubtree2 dad
    return $! [cd m] ++ [cm d]

--------------------------------------------------------------------------------
-- | Homologe Baumtausch-Rekombination. Verwendet Kontexte.
homologousTreeCrossover :: NFAC c Int => RX c -> RX c 
                        -> IO [ RX c ]
homologousTreeCrossover mom dad = do
    (cm, m) <- S.randomHomologousSubtree mom dad
    (cd, d) <- S.randomHomologousSubtree dad mom
    return $ [cd m] ++ [cm d]
