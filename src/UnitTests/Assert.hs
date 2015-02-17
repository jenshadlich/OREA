{-# language FlexibleContexts #-}

module UnitTests.Assert where

--------------------------------------------------------------------------------
assert b msg = do
    if b 
        then return ()
        else putStrLn $ "Test " ++ msg ++ " failed." 

--------------------------------------------------------------------------------
assertEqual msg a b = do
    putStrLn $ "assertEqual: " ++ show a ++ ", " ++ show b
    assert (if a == b then True else False) msg 

--------------------------------------------------------------------------------
assertTrue msg b = do
    putStrLn $ "assertTrue: " ++ show b
    assert (if b then True else False) msg 

--------------------------------------------------------------------------------
assertLq msg a b = do
    putStrLn $ "assertLq: " ++ show a ++ ", " ++ show b
    assert (if a >= b then True else False) msg 
