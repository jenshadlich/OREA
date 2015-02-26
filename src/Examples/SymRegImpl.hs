{-# language NoMonomorphismRestriction #-}
module Examples.SymRegImpl where

import Genetic.Types
import qualified Util.Random as R
import Examples.SymRegTypes

--------------------------------------------------------------------------------
-- | Create a random SRT
generate :: [Double] -> Genpool SRT -> Int
         -> IO (SRT)
generate crange gp depth = do
    if depth > 1
        then do
            r <- R.one $ functions gp
            case r of Fun1 op -> unary op; Fun2 op -> binary op
        else do
            b <- R.one [True, False]
            r <- if b -- ^ use terminal (true) or constant (false)
                then R.one $ terminals gp 
                else do c <- R.one crange; return $ Const c
            return r
    where
        binary op = do
            a <- generate crange gp (depth-1)
            b <- generate crange gp (depth-1)
            return $ op a b
        unary op = do
            a <- generate crange gp (depth-1)
            return $ op a

--------------------------------------------------------------------------------
-- | Fitness function
-- compute the 2-norm of the error of the function values
-- take length of SRT into account (shorter is better)
evaluate :: [Double] -> Genpool SRT -> [Double] -> SRT
         -> Double
evaluate xrange _ t i = (norm2 res) + (0.001 * fromIntegral (length $ show i)) 
    where
        decode = (\t -> (map (\x -> eval x t)) xrange)
        is = decode i
        res = map (\(x, y) -> x - y) (zip t is)  
        norm2 xs = sqrt $ sum (map abs xs)

--------------------------------------------------------------------------------
-- | Do a random tree recombination
crossover :: SRT -> SRT
          -> IO ([SRT])
crossover mom dad = do 
    (cm, m) <- randomSubtree mom
    (cd, d) <- randomSubtree dad
    return $ [cd m] ++ [cm d]

--------------------------------------------------------------------------------
-- | Mutate
mutate :: [Double] -> Genpool SRT -> SRT
       -> IO (SRT)
mutate crange gp i = do
    let muts = [subtreeN 3]
    m <- R.one muts
    m crange gp i

--------------------------------------------------------------------------------
-- | Random tree mutation
subtreeN :: Int -> [Double] -> Genpool SRT -> SRT
         -> IO (SRT)
subtreeN n crange gp i = do
    (c, _) <- randomSubtree i
    new <- generate crange gp n
    return $ c new 

--------------------------------------------------------------------------------
-- | SRT interpreter
eval :: Double -> SRT
     -> Double
eval x xs = 
    case xs of 
        Add   a b -> (eval x a) + (eval x b)
        Sub   a b -> (eval x a) - (eval x b)
        Mul   a b -> (eval x a) * (eval x b)
        Div   a b -> do                                 -- safe division
            let a' = (eval x b)
            let b' = (eval x b)
            if (abs b') <= 0.001 then a' else a' / b'   -- [PLM08], p. 156
        Sin   a   -> sin (eval x a)
        Sqr   a   -> a' * a' where a' = (eval x a)
        Sqrt  a   -> sqrt (eval x a)
        Const a   -> a
        VarX      -> x

--------------------------------------------------------------------------------
randomSubtree :: SRT
              -> IO (CSRT, SRT)
randomSubtree t = do
    rst <- R.one (subtrees t)
    return rst

--------------------------------------------------------------------------------
subtrees :: SRT
         -> [(CSRT, SRT)]
subtrees rx = trav root  ++ [root]
    where
    root = (id, rx) 
    trav :: (CSRT, SRT) -> [(CSRT, SRT)]
    trav (c, rx') =
        case rx' of
            Add   a b -> binary Add a b
            Sub   a b -> binary Sub a b
            Mul   a b -> binary Mul a b
            Div   a b -> binary Div a b
            Sin   a   -> unary  Sin a
            Sqr   a   -> unary  Sqr a
            Sqrt  a   -> unary  Sqr a
            Const _   -> []
            VarX      -> []
            where
                unary op a =
                    let cx = (\s -> c (op s)) in
                    (trav (cx, a)) ++ [(cx, a)]
                binary op a b = 
                    let cxL = (\s -> c (op s b))
                        cxR = (\s -> c (op a s)) in
                    (trav (cxL, a)) ++ (trav (cxR, b)) ++ [(cxL, a), (cxR, b)]
