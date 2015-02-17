@echo off
SET GHC_CALL=ghc --make -fhpc -fforce-recomp -main-is
echo "------------------"
echo "run the programms:"
echo "------------------"
%GHC_CALL% UnitTests.Genetic.Selections UnitTests\Genetic\Selections.hs
UnitTests\Genetic\Selections.exe

%GHC_CALL% UnitTests.Genetic.Threshold UnitTests\Genetic\Threshold.hs
UnitTests\Genetic\Threshold.exe

%GHC_CALL% UnitTests.Regex.Eval UnitTests\Regex\Eval.hs
UnitTests\Regex\Eval.exe

%GHC_CALL% UnitTests.Regex.Generate UnitTests\Regex\Generate.hs
UnitTests\Regex\Generate.exe

%GHC_CALL% UnitTests.Regex.Subtree UnitTests\Regex\Subtree.hs
UnitTests\Regex\Subtree.exe

%GHC_CALL% UnitTests.Regex.Mutation UnitTests\Regex\Mutation.hs
UnitTests\Regex\Mutation.exe

%GHC_CALL% UnitTests.Regex.Crossover UnitTests\Regex\Crossover.hs
UnitTests\Regex\Crossover.exe

%GHC_CALL% UnitTests.Util.Analysis UnitTests\Util\Analysis.hs
UnitTests\Util\Analysis.exe

%GHC_CALL% UnitTests.Util.Levenshtein UnitTests\Util\Levenshtein.hs
UnitTests\Util\Levenshtein.exe

echo "--------------"
echo "create markup:"
echo "--------------"
SET TIX_FILES=Selections.exe.tix Threshold.exe.tix Eval.exe.tix Generate.exe.tix Subtree.exe.tix Mutation.exe.tix Crossover.exe.tix Analysis.exe.tix Levenshtein.exe.tix
hpc sum --union --output Test.sum.tix %TIX_FILES%
hpc markup Test.sum.tix --exclude=UnitTests.* --hpcdir=".hpc/" --srcdir="." --destdir=".hpc/hpcMarkupAll"
hpc markup Test.sum.tix --exclude=UnitTests.* --hpcdir=".hpc/" --srcdir="." --destdir=".hpc/hpcMarkupGenetic" Genetic.Central Genetic.Selection.Environmental Genetic.Selection.Parental Genetic.Selection.RandomIndividuum
hpc markup Test.sum.tix --exclude=UnitTests.* --hpcdir=".hpc/" --srcdir="." --destdir=".hpc/hpcMarkupRegex" Regex.Eval Regex.Generate Regex.Ops.Crossover Regex.Ops.Mutation Regex.Subtree Regex.Util
hpc markup Test.sum.tix --exclude=UnitTests.* --hpcdir=".hpc/" --srcdir="." --destdir=".hpc/hpcMarkupUtil" Util.Analysis Util.Levenshtein Util.Parallel Util.Random

echo "--------------"
echo "create report:"
echo "--------------"
hpc report Test.sum.tix --hpcdir=".hpc/"

del UnitTests\Genetic\*.hi
del UnitTests\Genetic\*.o
del UnitTests\Genetic\*.exe
del UnitTests\Regex\*.hi
del UnitTests\Regex\*.o
del UnitTests\Regex\*.exe
del UnitTests\Util\*.hi
del UnitTests\Util\*.o
del UnitTests\Util\*.exe
del Test.sum.tix %TIX_FILES%