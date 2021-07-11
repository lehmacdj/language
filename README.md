# language

This is my primary language project. I intend to use it as a playground to
add/implement features that I am interested by. This language doesn't aim to be
particularly coherent or usable, just to be useful to me as a playground for
implementing ideas related to compiler development.

## Compilation Pipeline
1. Parsing
   - Creates AST from source code.
   - Extremely basic reconstruction is performed in this phase. i.e.
     `lambda x. y` is reconstructed to `lambda x : _. y`.
2. Reconstruction
   - the source language may not require everything to be written out
     explicitly. Terms left out need to be reconstructed to obtain what was
     actually meant.
   - For example consider this typical definition of the identity function in
     Haskell:
     ```
     id :: a -> a
     id x = x
     ```
     Here we don't explcitly quantify over the type variable `a` and we also
     don't take an argument for this quantifier. A "reconstructed" program might
     look like
     ```
     id :: forall a :: *. a -> a
     id @_ x = x
     ```
     where we have created both a closed type and a closed term for `id` that
     can now be type checked without running into unbound variables.
   - At first we will probably ignore reconstruction and skip straight to
     elaboration, as reconstruction doesn't actually provide too strong of
     syntactic conveniences. This does mean that we will need to explicitly pass
     at least an `_` meaning an inferred value as explicit type applications.
3. Elaboration
   - produces a set of constraints decorated with unification variables
4. Unification / Inference
   - uses Huet's unification algorithm to create substitutions for inferred terms
    as well as possible
   - Definitely undecideable, but we'll try our best and impose limits using
    fuel to make sure that type checking generally terminates
5. Type Checking
   - extremely dumb type checking algorithm that doesn't try to do any kind of
    inference
6. Interpretation / Optimization + Compilation

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
- pattern matching including view patterns
- (co)effect system
- Meta programming based off of lambda star
  - possibly can provide an interface for theorem proving when unification isn't
    strong enough on its own
  - ideally unification variables can be meta variables
