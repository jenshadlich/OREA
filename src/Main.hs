{-# language FlexibleContexts, ScopedTypeVariables #-}
-- | Hauptprogramm zur Optimierung regulaerer Ausdruecke.
module Main where

import Autolib.Exp.Type
import Genetic.Central
import Genetic.Types
import Genetic.Trace

import qualified Regex.Eval
import qualified Regex.Ops.Mutation
import qualified Regex.Ops.Crossover
import qualified Regex.Generate
import qualified Regex.Util as RU
import qualified Regex.Trace as T
import qualified Util.Time
import OREA.Xml
import System.Environment (getArgs)

{-
Sigma | Regex               | f5, f6 | optimale Loesung
--------------------------------------------------------------------------------
 abc  | ( ab $ bc )^*    HS |   32.0 | (1)
 ab   | a^2^* $ b^2^*    HS |   33.0 | (2)
 abc  | (ab)^* $ (bc)^*  HS |   29.0 | (3)
 abc  | (ab)^* $ c^*        |    9.0 | (c + a c^* b)^*
 ab   | a^2^* $ b^*         |    9.0 | (a b^* a + b)^*
 ab   | ( ab + bb )^*       |    6.0 | ((b + a) b)^*
 ab   | ( a + b )^*         |    4.0 | siehe Regex
 abc  | ab $ bc             |   39.0 | (4)
 
 HS = HIGHSCORE-Aufgabe, BBL = beste bekannte Loesung
 
 (1) BBL: (ba (cb + bc)  + ab (bc + cb) + bcab)^*
 (2) BBL: (bb + aa + (ab + ba) (bb + aa)^* (ab + ba))^*
 (3) BBL: (b (ab)^* (c + ac b (cb)^*) + a b (cb)^*)^*
 (4) bcab + bacb + babc + abbc + abcb
-}
--------------------------------------------------------------------------------
-- | default-Wert fuer Alphabet
defSigma :: String
defSigma = "abc" 
-- | default-Wert fuer regulaeren Ausdruck E mit L(E) = Z 
defRegex :: String
--defRegex = "(ab)^* $ c^*"
--defRegex = "a^2^* $ (b + c)^* - All b a^* b All"
defRegex = "a^2^* $ (b + c)^*" -- (a (b + c)^* a + b + c)^*
--defRegex = "All b a^* b All" -- ((c + a + b)^* b a^* b (a + c + b)^*)
--defRegex = "All - ((c + a + b)^* b a^* b (a + c + b)^*)" -- ((a^* b a^* + a^*) c)^* (a^* b a^* + a^*)
-- | default-Wert Dateiname fuer XML-Ausgabe
defXmlOutFile :: String
defXmlOutFile = "orea.xml"

--------------------------------------------------------------------------------
-- | Hauptprogramm zur Optimierung regulaerer Ausdruecke
main :: IO()
main = do
    args <- getArgs
    let (sigma, regex, xmlOutFile) = readCmdArgs args

    let gp = Genpool { -- einfacher regulaerer Ausdruck (*, +, .) 
        terminals = map Letter sigma,
        functions = map Fun1 [PowerStar] ++ map Fun2 [Union, Dot] 
    }

    let conf = Configuration { --  GP-Instanz 
        --Parameter:
        maxGenerations    = 1000,
        maxPopulationSize = 50,
        threshold         = Just 13,
        phenotype         = decode rx,
        genpool           = gp,
        seed              = Nothing,
        --Selektionsmodi:
        parSelection      = Tournament 0.5 3,      
        envSelection      = Best100Unique,
        --Funktionen:
        fDecode           = decode,
        fFitness          = (\_ pt i -> Regex.Eval.evaluate6 pt i),
        --fTracePop         = tracePrintStats,
        fTracePop         = (\n p ->
            do T.traceMissingWords n p 5; writeXmlP xmlOutFile n p),
        fGenerate         = (\gp' -> Regex.Generate.generateRX gp' 5),
        fMutate           = Regex.Ops.Mutation.mutate3,
        fCrossover        = Regex.Ops.Crossover.randomTreeCrossover
    } where
        decode = (\i -> RU.toNFA2 sigma' i)
        (sigma', rx) = RU.mkRX sigma regex

    printHeader sigma regex
    writeXmlHeader xmlOutFile
    
    (cTime, result) <- Util.Time.stopTime (evolve conf)
    showWinner result

    writeXmlFooter xmlOutFile cTime
    return ()

--------------------------------------------------------------------------------
-- | Kommandozeilenargumente lesen
-- falls Argumente nicht gesetzt, default-Werte verwenden
readCmdArgs :: [String]
            -> (String, String, String)
readCmdArgs (a:b:c:_) = (a,        b,        c)
readCmdArgs (a:b:_)   = (a,        b,        defXmlOutFile)
readCmdArgs (a:_)     = (a,        defRegex, defXmlOutFile)
readCmdArgs _         = (defSigma, defRegex, defXmlOutFile)

--------------------------------------------------------------------------------
-- ! Kopfzeile mit regulaerem Ausdruck und Alphabet ausgeben
printHeader :: String -> String
            -> IO ()
printHeader s r = do
    putStrLn $ "==================================================="
    putStrLn $ "Regex = " ++ r ++ "\t" ++ "Sigma = " ++ s
    putStrLn $ "==================================================="
    putStrLn ""
