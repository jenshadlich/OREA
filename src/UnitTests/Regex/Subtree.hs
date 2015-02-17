{-# language FlexibleContexts #-}

module UnitTests.Regex.Subtree where

import UnitTests.Assert

-- Dependancies
import Autolib.Exp.Inter
import Autolib.Exp.Type
import qualified Autolib.Exp.Syntax (subtrees)
import Autolib.NFA hiding (Dot)
import Genetic.Types
import Regex.Util

-- Module to test
import Regex.Subtree

--------------------------------------------------------------------------------
testMultiStars = do
    let (_, rx) = mkRX "ab" "(a(a)^*^* + (bba + abb)^*)^*^*"
    assertTrue  ""   (hasMultiStars rx)
    assertEqual "" 2 (countMultiStars rx)
    return ()

--------------------------------------------------------------------------------
testSubtree = do
    let (_, rx) = mkRX "ab" "(aa + bb)^*"
    let (_, ry) = mkRX "ab" "(aa + ab^*a + a^*bbaabba^*)^*"

    (_, r) <- randomSubtree2 rx
    assertTrue (show r) (elem r (Autolib.Exp.Syntax.subtrees rx))

    (_, s) <- randomSubtree2 ry
    assertTrue (show s) (elem s (Autolib.Exp.Syntax.subtrees ry))

    t <- randomSubtree rx
    assertTrue (show t) (elem t (Autolib.Exp.Syntax.subtrees rx))

    return ()

--------------------------------------------------------------------------------
testLeaf = do
    let (_, rx) = mkRX "ab" "(aa + bb)^*"
    (_, r) <- randomLeaf rx
    assertTrue "" (elem r  (map (\x -> snd $ mkRX "ab" x) ["a", "b"]))
    return ()

--------------------------------------------------------------------------------
testHSubtree = do
    let (_, rx) = mkRX "ab" "(a + bb)^*"
    let (_, ry) = mkRX "ab" "(aa + b^*)^*"
    (_, b) <- randomHomologousSubtree rx ry
    let rs = map (\x -> snd $ mkRX "ab" x) ["(a + bb)^*", "a", "a + bb", "bb"]
    assertTrue "" (elem b rs)
    return ()

--------------------------------------------------------------------------------
testGlue = do
    o <- randomGlueOp
    assertTrue "" True -- TODO

--------------------------------------------------------------------------------
-- | Main
main = do
    testSubtree 
    testLeaf
    testHSubtree
    testMultiStars
    testGlue