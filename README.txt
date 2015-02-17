================================================================================
                                  README
================================================================================

* Kompilieren / Laufzeit-Einstellungen:
---------------------------------------

- normal: ghc --make Main.hs
- profiling an: ghc -prof -auto-all -caf-all -fforce-recomp --make Main.hs
                ./Main +RTS -p -hc -sstderr
                ./Main +RTS -p -hy -sstderr
                ./Main +RTS -p -xt -hc -i10 -sstderr
- optimiert: ghc --make -O2 -threaded -fforce-recomp Main.hs
             ./Main +RTS -N4

- _alle_ Libraries müssen mit profiling installiert worden sein,
  z.B. per: cabal install autolib-fa -p --reinstall

--------------------------------------------------------------------------------