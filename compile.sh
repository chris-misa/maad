#/usr/bin/env bash

ghc -O2 -prof -fprof-auto Singularities.hs -main-is Singularities
