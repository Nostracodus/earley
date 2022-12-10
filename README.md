
This visualizer is [available here](http://nostracodus.github.io/earley).

# Description

This is a tool that implements the Earley parsing algorithm as given by Elizabeth Scott and Adrian Johnstone in Recognition is not parsing — SPPF-style parsing from cubic recognisers.
Traditional Earley parser algorithms are based on adding links between items and walking the links post-parse to construct some form of derivation. This algorithm instead builds a derivation graph during parsing. 

# Useful Links

[Earley Parsing Explained](https://loup-vaillant.fr/tutorials/earley-parsing/). This is a particularly approachable description of the Earley algorithm.

[A Pint-sized Earley Parser](https://joshuagrams.github.io/pep/). Another implementation of the SPPF-building algorithm (with explanation).