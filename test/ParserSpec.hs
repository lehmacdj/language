module ParserSpec where

import AST
import Parser
import Test.QuickCheck.Instances.Natural ()
import TestPrelude
import Text.Megaparsec

test_pTerm :: TestTree
test_pTerm =
  testGroup
    "pTerm"
    [ "U0" `parsesTo` Universe 0,
      "U123" `parsesTo` Universe 123,
      "U9001" `parsesTo` Universe 9001,
      "U0x" `failsBecause` "space required for function application",
      "magic" `parsesTo` Magic,
      "magicx" `failsBecause` "space required for function application",
      "x" `parsesTo` Var "x",
      "xU0" `parsesTo` Var "xU0",
      "xmagic" `parsesTo` Var "xmagic",
      "-" `failsBecause` "variables can't start with -",
      "'" `failsBecause` "variables can't start with '",
      "_" `failsBecause` "variables can't start with _",
      "\"@*$&#.\"" `parsesTo` Var "@*$&#.",
      "forall x : y. z" `parsesTo` pib "x" (Var "y") (Var "z"),
      "forall x : y. x" `parsesTo` pib "x" (Var "y") (Var "x"),
      "wforall x : y. z" `failsBecause` "ill formed forall",
      "forallx : y. z" `failsBecause` "space required between forall and variable",
      "lambda x. y" `parsesTo` lam "x" (Var "y"),
      "xlambda y. z" `failsBecause` "ill formed lambda",
      "lambdax. y" `failsBecause` "space required between lambda and variable",
      "x -> y" `parsesTo` (Var "x" `arrow` Var "y"),
      "x -> y -> z" `parsesTo` (Var "x" `arrow` (Var "y" `arrow` Var "z")),
      "(x -> y) -> z" `parsesTo` ((Var "x" `arrow` Var "y") `arrow` Var "z"),
      "x : y" `parsesTo` (Var "x" `TyAnn` Var "y"),
      "x -> y : z" `parsesTo` (Var "x" `arrow` (Var "y" `TyAnn` Var "z")),
      "(((x)))" `parsesTo` Var "x",
      "x y" `parsesTo` (Var "x" `App` Var "y"),
      "x(y)" `parsesTo` (Var "x" `App` Var "y"),
      "x y z" `parsesTo` ((Var "x" `App` Var "y") `App` Var "z"),
      "x y -> z" `parsesTo` ((Var "x" `App` Var "y") `arrow` Var "z"),
      "x : y -> z" `parsesTo` ((Var "x" `TyAnn` Var "y") `arrow` Var "z"),
      -- parenthesized lambdas / pi binders should work in the obvious way
      "(forall x1 : x2. x3) -> y" `parsesTo` (pib "x1" (Var "x2") (Var "x3") `arrow` Var "y"),
      "(lambda x1 . x2) -> y" `parsesTo` (lam "x1" (Var "x2") `arrow` Var "y"),
      "(forall x1 : x2. x3) y" `parsesTo` (pib "x1" (Var "x2") (Var "x3") `App` Var "y"),
      "(lambda x1 . x2) y" `parsesTo` (lam "x1" (Var "x2") `App` Var "y"),
      "x (forall y1 : y2. y3)" `parsesTo` (Var "x" `App` pib "y1" (Var "y2") (Var "y3")),
      "x (lambda y1 . y2)" `parsesTo` (Var "x" `App` lam "y1" (Var "y2")),
      "x -> (forall y1 : y2. y3)" `parsesTo` (Var "x" `arrow` pib "y1" (Var "y2") (Var "y3")),
      "x -> (lambda y1 . y2)" `parsesTo` (Var "x" `arrow` lam "y1" (Var "y2")),
      "x : (forall y1 : y2. y3)" `parsesTo` (Var "x" `TyAnn` pib "y1" (Var "y2") (Var "y3")),
      "x : (lambda y1 . y2)" `parsesTo` (Var "x" `TyAnn` lam "y1" (Var "y2")),
      "(forall x1 : x2. x3) : y" `parsesTo` (pib "x1" (Var "x2") (Var "x3") `TyAnn` Var "y"),
      "(lambda x1 . x2) : y" `parsesTo` (lam "x1" (Var "x2") `TyAnn` Var "y"),
      -- when un parenthesized forall and lambda should be taken to extend all
      -- of the way to the right. But they can be used as arguments to infix
      -- operators or function applications
      "forall x1 : x2. x3 y" `parsesTo` pib "x1" (Var "x2") (Var "x3" `App` Var "y"),
      "lambda x1 . x2 y" `parsesTo` lam "x1" (Var "x2" `App` Var "y"),
      "x forall y1 : y2. y3" `parsesTo` (Var "x" `App` pib "y1" (Var "y2") (Var "y3")),
      "x lambda y1 . y2" `parsesTo` (Var "x" `App` lam "y1" (Var "y2")),
      "forall x1 : x2. x3 -> y" `parsesTo` pib "x1" (Var "x2") (Var "x3" `arrow` Var "y"),
      "lambda x1 . x2 -> y" `parsesTo` lam "x1" (Var "x2" `arrow` Var "y"),
      "x -> forall y1 : y2. y3" `parsesTo` (Var "x" `arrow` pib "y1" (Var "y2") (Var "y3")),
      "x -> lambda y1 . y2" `parsesTo` (Var "x" `arrow` lam "y1" (Var "y2")),
      "forall x1 : x2. x3 : y" `parsesTo` pib "x1" (Var "x2") (Var "x3" `TyAnn` Var "y"),
      "lambda x1 . x2 : y" `parsesTo` lam "x1" (Var "x2" `TyAnn` Var "y"),
      "x : forall y1 : y2. y3" `parsesTo` (Var "x" `TyAnn` pib "y1" (Var "y2") (Var "y3")),
      "x : lambda y1 . y2" `parsesTo` (Var "x" `TyAnn` lam "y1" (Var "y2"))
    ]
  where
    parsesTo x y =
      testCase (show x <> " parses to " <> show y) $
        testParserParses (pTerm <* eof) x y
    failsBecause x r =
      testCase (show x <> " parsing fails") $
        testParserFails (pTerm <* eof) x r
