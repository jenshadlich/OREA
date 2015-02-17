{-# language FlexibleContexts #-}
module Regex.Eval where

import Autolib.Exp.Type 
import Autolib.NFA
import Autolib.Size
import qualified Autolib.NFA.Minus as M
import Genetic.Types

import qualified Regex.Util as U
import qualified Regex.Subtree as S

--------------------------------------------------------------------------------
-- | Experimentelle Bewertungsfunktion mit ueberpruefung des Kontext des NFA.
evaluate5 :: (NFAC c Int, Size (RX c))
          => NFA c Int -> RX c
          -> Double
evaluate5 target indiv =
    let semantics = U.toNFA indiv
        ccFalse = not $ contextCheck target semantics
        missing = M.minus target semantics
        wmiss = weight missing
        weight a = sum $ do
            w <- take 100 $ accepted a
            return $ 2 ^^ negate ( fromIntegral ( length w) )

    in      10000 * (if ccFalse then 1 else 0)
        +    1000 * wmiss
        +     100 * fromIntegral (S.countMultiStars indiv)
        +       1 * fromIntegral (size indiv)

--------------------------------------------------------------------------------
-- | Bewertungsfunktion f6.
evaluate6 :: (NFAC c Int, Size (RX c))
          => NFA c Int -> RX c
          -> Double
evaluate6 target indiv =
    let semantics = U.toNFA indiv -- aus RX NFA konstr.
        toomuch = M.minus semantics target     -- Automat d. ueberzaehlige Woerter 
        missing = M.minus target semantics     -- Automat d. fehlende Woerter
        feasible = isFeasible target semantics -- Test auf Korrektheit
        
        -- Gewichtung: die ersten 100 durch den Automtaten a akzeptierten
        -- Woerter w werden nach deren Laenge bewertet mit ( 2^{ - |w| } )
        -- z.B. |w| = 1 -> 2^-1
        --      |w| = 2 -> 2^-2 
        --      |w| = 3 -> 2^-3
        weight a  = sum $! do
            w <- take 100 $! accepted a
            return $! 2 ^^ negate ( fromIntegral ( length w) )

    -- Fitnessformel:
    in   100000 * weight toomuch  -- gewichtete Bewertung ueberzaehliger Woerter 
       +   1000 * weight missing  -- gewichtete Bewertung fehlender Woerter
       +    100 * fromIntegral (S.countMultiStars indiv) -- Bestrafung von ^*^*
       +      1 * fromIntegral (size indiv)         -- Laenge des RX
       +      1 * if (not feasible) then 100 else 0 -- +100 wenn nicht korrekt

--------------------------------------------------------------------------------
-- Hilfsfunktionen
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
contextCheck :: NFAC c Int
              => NFA c Int ->  NFA c Int
              -> Bool
contextCheck target x = or $ do
    s <- lstates target
    t <- lstates target
    let a = target { starts = mkSet [s], finals = mkSet [t] }
    return $ isSubsetOf x a

--------------------------------------------------------------------------------
isSubsetOf :: NFAC c Int
           => NFA c Int ->  NFA c Int
           -> Bool
isSubsetOf a b = null $ some_shortest (minus a b)

--------------------------------------------------------------------------------
-- | ueberprueft ob RX i brauchbar (= feasible) ist, d.h. ob er diesselbe
-- Sprache akzeptiert wie der Ziel-NFA
isFeasible :: NFAC c Int
           => NFA c Int -> NFA c Int 
           -> Bool
isFeasible t i = 
    case some_shortest (symdiff t i) of
        [] -> True
        _  -> False
