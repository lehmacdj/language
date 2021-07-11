# language

This is my primary language project. I intend to use it as a playground to
add/implement features that I am interested by. This language doesn't aim to be
particularly coherent or usable, just to be useful to me as a playground for
implementing ideas related to compiler development.

## Compilation Pipeline
1. Parsing
2. Elaboration
   - produces a set of constraints decorated with unification variables
3. Unification / Inference
   - uses Huet's unification algorithm to create substitutions for inferred terms
    as well as possible
   - Definitely undecideable, but we'll try our best and impose limits using
    fuel to make sure that type checking generally terminates
4. Type Checking
   - extremely dumb type checking algorithm that doesn't try to do any kind of
    inference
5. Interpretation / Optimization + Compilation

## TODO
- Elaborator
- Unification engine
- Pretty printer
- Revamp error messages
  - Error messages need to have textual representations that make sense
  - have helper methods to convert polymorphic variables into something readable
    for the end user
  - context traces for type checking to make messages more readable. This is
    similar to how GHC does it:
    ```
    type mismatch:
    - expected: foo
    - but was:  bar
    in blah blah blah
    in baz (blah blah blah)
    ```

## Far future ideas
- Meta programming based off of lambda star
  - possibly can provide an interface for theorem proving when unification isn't
    strong enough on its own
  - ideally unification variables can be meta variables
