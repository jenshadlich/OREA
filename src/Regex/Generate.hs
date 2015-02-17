{-# language FlexibleContexts #-}
module Regex.Generate (generateRX) where

import Autolib.Exp.Type
import Autolib.NFA hiding (Dot)
import Autolib.Util.Zufall (eins)
import Genetic.Types
import qualified Util.Random as R

--------------------------------------------------------------------------------
-- | Zufaelligen regulaeren Ausdruck (RX) entsprechend Genpool und max. Tiefe
-- erzeugen.
generateRX :: NFAC c Int => Genpool (RX c) -> Int -> IO (RX c)
generateRX gp depth = do
    if depth > 1
        then do
            r <- R.one $ functions gp
            case r of 
                Fun1 op -> unary op
                Fun2 op -> binary op
        else do
            r <- eins $ terminals gp
            return r
    where
        binary op = do
            a <- generateRX gp (depth-1)
            b <- generateRX gp (depth-1)
            return $ op a b
        unary op = do
            a <- generateRX gp (depth-1)
            return $ op a
