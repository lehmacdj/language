# language

This is my primary language project. I intend to use it as a playground to
add/implement features that I am interested by. This language doesn't aim to be
particularly coherent or usable, just to be useful to me as a playground for
implementing ideas related to compiler development.

## Compilation Pipeline
1. Parsing
2. Elaboration (currently no-op; goal is to add extra type annotations as much
   as possible, probably based off of some constraint solving system similar to
   Hindley-Milner type inference in order to make type annotation + polymorphic
   instantiation less burdensome)
3. Type Checking
4. Interpretation / Optimization + Compilation
