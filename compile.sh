#/usr/bin/env bash

#ghc -O2 -prof -fprof-auto Singularities.hs -main-is Singularities
ghc -O2 Singularities.hs -main-is Singularities

ghc -O2 -prof -fprof-auto StructureFunction.hs -main-is StructureFunction
#ghc -O2 StructureFunction.hs -main-is StructureFunction

#ghc -O2 -prof -fprof-auto Spectrum.hs -main-is Spectrum
ghc -O2 Spectrum.hs -main-is Spectrum
