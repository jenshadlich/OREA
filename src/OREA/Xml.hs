{-# language FlexibleContexts, ScopedTypeVariables #-}
module OREA.Xml (writeXmlHeader, writeXmlFooter, writeXmlP) where

import Autolib.Exp.Type
import Autolib.Symbol
import Autolib.Size
import Genetic.Types

import qualified Regex.Util as RU
import qualified Util.Analysis as A

--------------------------------------------------------------------------------
-- | Ausgabe der Population (= aktuelle Generation) als XML gemaess orea.dtd.
-- Dabei werden statistische Informationen (durchschnittliche Fitness,
-- empirische Varianz der Fitness; minmale, max. und durchschnittliche Groesse
-- der Individuuen) berechnet und alle Individuen mit ihrem Genotyp (als String)
-- und deren Fitness ausgegeben.
writeXmlP :: (Symbol c, Size (RX c)) =>
          String -> Int -> Population pt (RX c) (Genpool (RX c)) Double
          -> IO ()
writeXmlP out n pop = do
    let generation = n
    appendFile out $
        "  <population" ++
        (xmlAttr "generation"     (generation)) ++
        (xmlAttr "size"           (length $ population pop)) ++
        (xmlAttr "avgFitness"     (A.averageFitness pop)) ++
        (xmlAttr "sampleVariance" (A.sampleVariance pop)) ++
        --(xmlAttr "diversity"      (A.diversity pop)) ++
        (xmlAttr "minSize"
            (foldr1 min (map (\x -> size $ genotype x) $ population pop))) ++
        (xmlAttr "maxSize"
            (foldr1 max (map (\x -> size $ genotype x) $ population pop))) ++
        (xmlAttr "avgSize"        (
            (sum $ map (\x -> fromIntegral $ size $ genotype x) $ population pop)
            / fromIntegral (length (population pop))
            )) ++
        ">\n"
    appendFile out $ writeXmlI $ population pop
    appendFile out "  </population>\n"
    return ()

--------------------------------------------------------------------------------
-- | Indivuduum als XML.
writeXmlI :: (Symbol c, Size (RX c)) => [Individuum (RX c) Double]
          -> String
writeXmlI [] =  ""
writeXmlI (x:xs) = is ++ (writeXmlI xs)
    where
        is = "    " ++
             "<individuum" ++
             " genotype=\"" ++ (RU.serialize $ genotype x) ++ "\"" ++
             (xmlAttr "fitness"  (fitness x)) ++
             (xmlAttr "size"     (size $ genotype x)) ++
             "/>\n"

--------------------------------------------------------------------------------
-- | Hilfsfunktion um Werte in XML-Attribut (String) zu wandeln.
xmlAttr :: (Show a) => String -> a
        -> String
xmlAttr k v = " " ++ k ++ "=\"" ++ (show v) ++ "\""

--------------------------------------------------------------------------------
-- | Kopf fuer OREA-XML-Struktur.
writeXmlHeader :: String
               -> IO ()
writeXmlHeader out = do
    writeFile  out  $ "<?xml version=\"1.0\"?>\n"
    appendFile out $ "<!DOCTYPE orea SYSTEM \"orea.dtd\">\n"
    appendFile out $ "<orea>\n"

--------------------------------------------------------------------------------
-- | Fuss fuer OREA-XML-Struktur.
writeXmlFooter :: String -> Double
               -> IO ()
writeXmlFooter out cTime = do 
    appendFile out $ "  <computationTime seconds=\"" ++ show cTime ++ "\" />\n"
    appendFile out $ "</orea>"

