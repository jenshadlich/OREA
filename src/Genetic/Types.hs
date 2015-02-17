{-# language FlexibleContexts, ScopedTypeVariables #-}

module Genetic.Types where

--------------------------------------------------------------------------------
-- | Instanz fuer Gleichheitstest zweier Individuuen: Fitness ist Kriterium
instance (Eq f) => Eq (Individuum gt f) where
    a == b = fitness a == fitness b

--------------------------------------------------------------------------------
-- | Instanz fuer Ordnungsrelation: Sortierung aufsteigend nach Fitness
instance (Ord f) => Ord (Individuum gt f) where
    a <= b = fitness a <= fitness b

-------------------------------------------------------------------------------
-- | Datentyp fuer Individuum
data Individuum gt f = Individuum {
    genotype :: gt, -- ^ Genotyp
    fitness  :: f   -- ^ Fitness
} deriving (Show)

-------------------------------------------------------------------------------
-- | Datentyp fuer Population
data Population pt gt gp f = Population {
    population :: [Individuum gt f],        -- ^ Liste mit Individuuen
    config     :: Configuration pt gt gp f  -- ^ Aktuelle Konfiguration
}

-------------------------------------------------------------------------------
-- | Datentyp fuer zentrale Konfiguration der Instanz des 
-- evolutionaeren Algorithmus
data Configuration pt gt gp f = Configuration {
    --Attribute:
    genpool           :: gp,        -- ^ Genpool
    maxGenerations    :: Int,       -- ^ max. Anzahl Generationen
                                    -- (Abbruchkriterium)
    maxPopulationSize :: Int,       -- ^ max. Anzahl Individuen pro Population
    phenotype         :: pt,        -- ^ Phaenotyp
    threshold         :: Maybe f,   -- ^ Schwellwert (Abbruchkriterium)
    seed              :: Maybe [gt],-- ^ wird in Startpopulation eingeschleust
    -- Selektionsmodi:
    parSelection      :: ParentalSelection,     -- ^ Algorithmus Elternselektion
    envSelection      :: EnvironmentalSelection,-- ^ Algorithmus Umweltselektion
    -- Funktionen:
    fCrossover        :: gt -> gt -> IO ([gt]), -- ^ Interface Rekombination
    fDecode           :: gt -> pt,              -- ^ Interface Dekodierfunktion
    fFitness          :: gp -> pt -> gt -> f,   -- ^ Interface Bewertungsfunktion
    fGenerate         :: gp -> IO (gt),         -- ^ Interface Erzeuge-Individuum
    fMutate           :: gp -> gt -> IO (gt),   -- ^ Interface Mutation
    fTracePop         :: Int -> Population pt gt gp f -> IO ()  -- ^ Interface
                                                                -- Ausgabefunktion
}

-------------------------------------------------------------------------------
-- | Algorithmen zur Elternselektion.
-- Parameter Rekombinationswahrscheinlichkeit = (1 - Mutationsrate) aus
-- [0.00, 0.01 .. 1.00].
data ParentalSelection 
    = Linear Double         -- ^ linear entsprechend der Position in der
                            -- Liste Individuen (= Population);
                            -- Parameter: Rekombinationswahrscheinlichkeit
    | Tournament Double Int -- ^ Turnierselektion.
                            -- Parameter: Rekombinationswahrscheinlichkeit,
                            -- Turniertiefe 
    | FitnessProp Double    -- ^ Fitnessproportionale Selektion.
                            -- Parameter: Rekombinationswahrscheinlichkeit
    | FitnessPropWithTournament Double Int -- ^ Fitnessproportionale Selektion
                                           -- und anschliessendes Turnier.
                                           -- Parameter: Rekombinationswahrscheinlichkeit,
                                           -- Turniertiefe 
    deriving (Show)

-------------------------------------------------------------------------------
-- | Algorithmen zur Umweltselektion
data EnvironmentalSelection
    = Best100                   -- ^ Bestenselektion
    | Best100Unique             -- ^ Bestenselektion (Unikat)
    | Best80Random20            -- ^ Auswahl der besten 80 % und 20 % zufaellig
                                -- (experimentell)
    | NAryDoubleTournament Int  -- ^ q-stufiges zweifaches Turnier;
                                -- Parameter: Turniertiefe
    deriving (Show)

-------------------------------------------------------------------------------
-- | Datentyp fuer Genpool speziell fuer Genetische Programmierung: Container fuer
-- eine Menge von Terminalsymbolen bzw. Funktionen
data Genpool t = Genpool {
    terminals :: [t],          -- ^ Terminale
    functions :: [Function t]  -- ^ Funktionen
}

-------------------------------------------------------------------------------
-- | Datentyp fuer Funktionsmenge in Genpool
data Function t = Fun1 (t -> t)      -- ^ einstellige Funktion
                | Fun2 (t -> t -> t) -- ^ zweistellige Funktion
