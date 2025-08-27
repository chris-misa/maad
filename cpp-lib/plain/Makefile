CXX = g++
CXXFLAGS = -std=c++17 -O2

# Targets matching the Haskell executables
TARGETS = Singularities StructureFunction Spectrum Common
SOURCES = Singularities.cpp StructureFunction.cpp Spectrum.cpp Common.cpp

.PHONY: all clean test

all: $(TARGETS)

# Build each target from its corresponding source
Singularities: Singularities.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

StructureFunction: StructureFunction.cpp  
	$(CXX) $(CXXFLAGS) $< -o $@

Spectrum: Spectrum.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

Common: Common.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

test: Common
	./Common

clean:
	rm -f $(TARGETS)