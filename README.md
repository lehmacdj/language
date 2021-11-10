# language

This is my primary language project. I intend to use it as a playground to
add/implement features that I am interested by. This language doesn't aim to be
particularly coherent or usable, just to be useful to me as a playground for
implementing ideas related to compiler development.

## Compilation Pipeline
1. Parsing
   - Creates AST from source code.
   - Extremely basic reconstruction is currently performed in this phase. i.e.
     `lambda x. y` is reconstructed to `lambda x : _. y`.
   - Ideally, we will actually push all reconstruction into a separate
     reconstruction phase so the distinction is better defined and so it is
     possible to inject code at more discrete parts of the compilation pipeline.
   - When we no longer do reconstruction during parsing we will construct a
     concrete syntax tree (CST) during parsing that reflects exactly the
     structure of the source code
2. Reconstruction
   - the source language may not require everything to be written out
     explicitly.
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
   - Right now we don't have a reconstruction phase, things that should
     technically be in this phase occur during parsing. See also the note there.
4. Unification / Inference
   - first generate constraints based on the AST
   - uses Huet's unification algorithm to create substitutions for inferred
     terms as well as possible
   - Definitely undecideable, but we'll try our best and impose limits using
     fuel to make sure that type checking generally terminates
   - interested in seeing if we can provide guarantees that certain kinds of
     usage of the source language lead to decidable type checking in certain
     cases
5. Type Checking
   - extremely dumb type checking algorithm that doesn't try to do any kind of
     inference and is completely decidable
   - strictly speaking this isn't necessary, as the output of unification should
     be well typed. However the implementation of this is smaller and more
     obviously correct than the unification phase, so it has value especially if
     we have theorem proving capabilities through a termination co-effect
     eventually
6. Interpretation / Optimization + Compilation

### Notes on other compilation phase names that I'm not using
- Elaboration
  - Typically (e.g. in Agda + Idris) elaboration refers to lowering of a higher
    level language into a smaller core language.
  - Part of the goal of this language is to have 1ish language throughout so we
    don't have an explicit higher level language.
  - Reconstruction is essentially this phase, however I name it reconstruction
    to emphasize its separation from inference which is sometimes part of
    elaboration

## TODO
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
