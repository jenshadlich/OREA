{-# language FlexibleContexts, ScopedTypeVariables #-}
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
sigma | regex               | f5, f6 | optimal solution
--------------------------------------------------------------------------------
 abc  | ( ab $ bc )^*    HS |   32.0 | (1)
 ab   | a^2^* $ b^2^*    HS |   33.0 | (2)
 abc  | (ab)^* $ (bc)^*  HS |   29.0 | (3)
 abc  | (ab)^* $ c^*        |    9.0 | (c + a c^* b)^*
 ab   | a^2^* $ b^*         |    9.0 | (a b^* a + b)^*
 ab   | ( ab + bb )^*       |    6.0 | ((b + a) b)^*
 ab   | ( a + b )^*         |    4.0 | see regex
 abc  | ab $ bc             |   39.0 | (4)
 
 HS = HIGHSCORE task, BKS = best known solution
 
 (1) BKS: (ba (cb + bc)  + ab (bc + cb) + bcab)^*
 (2) BKS: (bb + aa + (ab + ba) (bb + aa)^* (ab + ba))^*
 (3) BKS: (b (ab)^* (c + ac b (cb)^*) + a b (cb)^*)^*
 (4) bcab + bacb + babc + abbc + abcb
-}
--------------------------------------------------------------------------------
-- | default value for sigma
defSigma :: String
defSigma = "abc" 
-- | default value for regular expression E with L(E) = Z
defRegex :: String
--defRegex = "(ab)^* $ c^*"
--defRegex = "a^2^* $ (b + c)^* - All b a^* b All"
defRegex = "a^2^* $ (b + c)^*" -- (a (b + c)^* a + b + c)^*
--defRegex = "All b a^* b All" -- ((c + a + b)^* b a^* b (a + c + b)^*)
--defRegex = "All - ((c + a + b)^* b a^* b (a + c + b)^*)" -- ((a^* b a^* + a^*) c)^* (a^* b a^* + a^*)
-- | default value xml output file
defXmlOutFile :: String
defXmlOutFile = "orea.xml"

--------------------------------------------------------------------------------
-- | main
main :: IO()
main = do
    args <- getArgs
    let (sigma, regex, xmlOutFile) = readCmdArgs args

    let gp = Genpool {
        terminals = map Letter sigma,
        functions = map Fun1 [PowerStar] ++ map Fun2 [Union, Dot] 
    }

    let conf = Configuration {
        -- config
        maxGenerations    = 1000,
        maxPopulationSize = 50,
        threshold         = Just 13,
        phenotype         = decode rx,
        genpool           = gp,
        seed              = Nothing,
        -- selection
        parSelection      = Tournament 0.5 3,      
        envSelection      = Best100Unique,
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
-- | cmd line arguments
-- use defaults if nothing is set
readCmdArgs :: [String]
            -> (String, String, String)
readCmdArgs (a:b:c:_) = (a,        b,        c)
readCmdArgs (a:b:_)   = (a,        b,        defXmlOutFile)
readCmdArgs (a:_)     = (a,        defRegex, defXmlOutFile)
readCmdArgs _         = (defSigma, defRegex, defXmlOutFile)

--------------------------------------------------------------------------------
-- ! print regex + sigma
printHeader :: String -> String
            -> IO ()
printHeader s r = do
    putStrLn $ "==================================================="
    putStrLn $ "Regex = " ++ r ++ "\t" ++ "Sigma = " ++ s
    putStrLn $ "==================================================="
    putStrLn ""
