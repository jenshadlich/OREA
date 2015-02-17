{-# language FlexibleContexts #-}

module UnitTests.Util.Levenshtein where

-- HUnit
import Test.HUnit

-- Zu testendes Modul
import Util.Levenshtein

--------------------------------------------------------------------------------
test1 = do TestCase $ assertEqual "" 2 (levenshtein "ab" "aa")

--------------------------------------------------------------------------------
test2 = do TestCase $ assertEqual "" 0 (levenshtein "aa" "aa")

--------------------------------------------------------------------------------
test3 = do TestCase $ assertEqual "" 8 (levenshtein "(a + b)^*" "a")

--------------------------------------------------------------------------------
-- | Main
main = runTestTT $ TestList [ test1, test2, test3 ] 
