{-# language NoMonomorphismRestriction #-}
module Examples.SymRegTypes where

--------------------------------------------------------------------------------
-- | Algebraic data type for symbolic regression that defines the available functions
data SRT = VarX          -- ^ variable x
         | Const Double  -- ^ constante
         | Add   SRT SRT -- ^ addition
         | Sub   SRT SRT -- ^ subtraction
         | Mul   SRT SRT -- ^ multiplication
         | Div   SRT SRT -- ^ division
         | Sin   SRT     -- ^ sin
         | Sqr   SRT     -- ^ square
         | Sqrt  SRT     -- ^ square root
         deriving (Eq)

--------------------------------------------------------------------------------
-- | context for SRT
type CSRT = SRT -> SRT

--------------------------------------------------------------------------------
-- | toString instance
instance Show SRT where
    show a = toString a

--------------------------------------------------------------------------------
-- | toString
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