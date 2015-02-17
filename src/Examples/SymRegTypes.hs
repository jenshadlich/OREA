{-# language NoMonomorphismRestriction #-}
module Examples.SymRegTypes where

--------------------------------------------------------------------------------
-- | algebraischer Datentyp fuer Symbolische Regression mit einigen wichtigen
-- mathematischen Operationen
data SRT = VarX          -- ^ Variable x
         | Const Double  -- ^ Konstante
         | Add   SRT SRT -- ^ Addition
         | Sub   SRT SRT -- ^ Subtraktion
         | Mul   SRT SRT -- ^ Multiplikation
         | Div   SRT SRT -- ^ Division
         | Sin   SRT     -- ^ Sinus
         | Sqr   SRT     -- ^ Quadratzahl
         | Sqrt  SRT     -- ^ Quadratwurzel
         deriving (Eq)

--------------------------------------------------------------------------------
-- | Kontext fuer SRT
type CSRT = SRT -> SRT

--------------------------------------------------------------------------------
-- | Instanz von Show um SRT in String umzuwandeln
instance Show SRT where
    show a = toString a

--------------------------------------------------------------------------------
-- | SRT in String umwandeln
toString :: SRT
         -> String
toString xs = 
    case xs of 
        Add   a b -> binary " + " a b
        Sub   a b -> binary " - " a b
        Mul   a b -> binary " * " a b
        Div   a b -> binary " / " a b
        Sin   a   -> "sin("  ++ (toString a) ++ ")"
        Sqr   a   -> "("     ++ (toString a) ++ ")^2"
        Sqrt  a   -> "sqrt(" ++ (toString a) ++ ")"
        Const a   -> show a
        VarX      -> "x"
    where
        binary op l r = "(" ++ (toString l) ++ op ++ (toString r) ++ ")"