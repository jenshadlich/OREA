{-# language FlexibleContexts, ScopedTypeVariables #-}
-- | Hauptprogramm zur Optimierung regulaerer Ausdruecke.
module Main where

import Autolib.Exp.Type
import Autolib.Exp.Inter
import Autolib.Size
import Autolib.NFA hiding (Dot)
import Autolib.NFA.Basic (sigmastar)
import qualified Autolib.NFA.Minus as M
import Genetic.Central
import Genetic.Types
import Genetic.Trace

--import qualified Regex.Eval
import qualified Regex.Ops.Mutation
import qualified Regex.Ops.Crossover
import qualified Regex.Generate
import qualified Regex.Subtree as S
import qualified Regex.Util as RU
import qualified Util.Time
import qualified Util.Analysis as A
import OREA.Xml
import System.Environment (getArgs)
import Text.Printf
--------------------------------------------------------------------------------
-- | default-Wert fuer Alphabet
defSigma :: String
defSigma = "01" 
-- | default-Wert Dateiname fuer XML-Ausgabe
defXmlOutFile :: String
defXmlOutFile = "orea.xml"

--------------------------------------------------------------------------------
evaluateMissing :: (NFAC c Int, Size (RX c))
                => Genpool (RX c) -> NFA c Int -> RX c -> [c]
                -> Double
evaluateMissing gp target indiv sigma =
    0
    + 100000 * weight toomuch
    +  10000 * smw
    +   1000 * smw2
    +    100 * if weight2 missing == 0 then 10000 else weight2 missing
    +    100 * fromIntegral (S.countMultiStars indiv)
{--
    +    100 * fromIntegral (S.countMultiStars indiv)
    +      1 * if isize > 42 then 50 else 0
    +      2 * if isize > 70 then fromIntegral isize else 0
    +      1 * if isize > 120 then 100000 else 0
--}
    +     10 * (fromIntegral $ round $ log $ fromIntegral $ isize )
    +      1 * fromIntegral isize
        where
            smw = 2 ^^ negate ( round $ log $ fromIntegral $ length $ shortestMissingWordI sigma indiv )
            smw2 = 2 ^^ negate ( length $ shortestMissingWordI sigma indiv )
            isize = size indiv
            semantics = RU.toNFA' (RU.sigmaFromGenpool gp) indiv
            toomuch = M.minus semantics target 
            missing = M.minus target semantics
            weight a  = sum $ do
                w <- take 25 $ accepted a
                return $ 2 ^^ negate ( fromIntegral ( length w ) )
            weight2 a  = sum $ do
                w <- take 25 $ accepted a
                return $ 1000 * ( 2 ^^ negate ( fromIntegral ( length w ) ) )

--------------------------------------------------------------------------------
traceShortestWord n sigma pop =
    printf "%4d\tf_avg = %10.3f\tf_min = %8.1f\tsmw = %s\n"
        n
        (A.averageFitness pop)
        (fitness $ head $ population pop)
        (shortestWord ++ " (lenght = " ++ show (length(shortestWord)) ++ ")")
    where
        shortestWord = shortestMissingWordP sigma pop    

--------------------------------------------------------------------------------
shortestMissingWordP sigma pop =
    shortestMissingWordI sigma (genotype $ head $ population pop)    

--------------------------------------------------------------------------------
shortestMissingWordI sigma indiv =
    let a = accepted $ M.minus (sigmastar sigma) (RU.toNFA' sigma indiv) in
    case a of
        (x:xs) -> head a
        []     -> []     
 
--------------------------------------------------------------------------------
-- | Hauptprogramm zur Optimierung regulaerer Ausdruecke
main :: IO()
main = do
    args <- getArgs
    let (sigma, xmlOutFile) = (defSigma, readCmdArgs args)

    let gp = Genpool { 
        --terminals = map Letter sigma ++ [read "All", read "Eps", read "Sigma"],
        --functions = map Fun1 [PowerStar] ++ map Fun2 [Union, Dot]
        terminals = map Letter sigma ++ [read "Eps", read "Sigma"],
        functions = map Fun1 [PowerStar] ++ map Fun2 [Union, Dot]
    }

    let conf = Configuration { --  GP-Instanz 
        maxGenerations    = 1000,
        maxPopulationSize = 5,
        threshold         = Nothing,
        phenotype         = sigmastar sigma,
        genpool           = gp,
        seed              = Nothing,
        parSelection      = Tournament 0.5 3,      
        envSelection      = Best100Unique,
        fDecode           = decode,
        fFitness          = \ gp target indiv -> evaluateMissing gp target indiv sigma,
        fTracePop         = \ n p -> 
            do
                traceShortestWord n sigma p;
                writeXmlP xmlOutFile n p,
        fGenerate         = \ gp' -> Regex.Generate.generateRX gp' 3,
        fMutate           = Regex.Ops.Mutation.mutate2,
        fCrossover        = Regex.Ops.Crossover.randomTreeCrossover2
    } where
        decode      = (\i -> RU.toNFA' sigma i)

    writeXmlHeader xmlOutFile
    
    (cTime, result) <- Util.Time.stopTime (evolve conf)
    showWinner result

    let shortestWord = shortestMissingWordP sigma result    
    
    putStrLn "______________________"
    putStrLn "Shortest missing word:"
    putStrLn $ shortestWord ++ " (length = " ++ show (length(shortestWord)) ++ ")"
         
    writeXmlFooter xmlOutFile cTime
    return ()

--------------------------------------------------------------------------------
-- | Kommandozeilenargumente lesen
-- falls Argumente nicht gesetzt, default-Werte verwenden
readCmdArgs :: [String]
            -> (String)
readCmdArgs (a:_)     = (a)
readCmdArgs _         = (defXmlOutFile)

--------------------------------------------------------------------------------
-- ! Kopfzeile mit regulaerem Ausdruck und Alphabet ausgeben
printHeader :: String -> String
            -> IO ()
printHeader s r = do
    putStrLn $ "==================================================="
    putStrLn $ "Regex = " ++ r ++ "\t" ++ "Sigma = " ++ s
    putStrLn $ "==================================================="
    putStrLn ""
