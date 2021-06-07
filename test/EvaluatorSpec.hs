module EvaluatorSpec where

import AST
import Evaluator
import TestPrelude

test_nf :: TestTree
test_nf =
  testGroup
    "nf"
    [ (lam "x" (Var "x") `App` Var "y") `hasNf` Var "y",
      -- note: bound normalizes variables internally so this kind of error
      -- isn't actually very problematic in practice
      (lam "x" (lam "y" (Var "x")) `App` Var "y") `hasNf` lam "x" (Var "y"),
      -- evaluation still happens under Pi and Lam
      pib "z" (Universe 0) (lam "x" (Var "x") `App` Var "y") `hasNf` pib "z" (Universe 0) (Var "y"),
      lam "z" (lam "x" (Var "x") `App` Var "y") `hasNf` lam "z" (Var "y")
    ]
  where
    hasNf :: Term' Text -> Term' Text -> TestTree
    t `hasNf` r = testCase ("nf $ " ++ show t) $ nf t @=? Right r

test_whnf :: TestTree
test_whnf =
  testGroup
    "whnf"
    [ (lam "x" (Var "x") `App` Var "y") `hasWhnf` Var "y",
      -- note: bound normalizes variables internally so this kind of error
      -- isn't actually very problematic in practice
      (lam "x" (lam "y" (Var "x")) `App` Var "y") `hasWhnf` lam "x" (Var "y"),
      -- whnf doesn't evaluate under Pi and Lam
      pib "z" (Universe 0) (lam "x" (Var "x") `App` Var "y") `hasWhnf` pib "z" (Universe 0) (lam "x" (Var "x") `App` Var "y"),
      lam "z" (lam "x" (Var "x") `App` Var "y") `hasWhnf` lam "z" (lam "x" (Var "x") `App` Var "y")
    ]
  where
    hasWhnf :: Term' Text -> Term' Text -> TestTree
    t `hasWhnf` r = testCase ("whnf $ " ++ show t) $ whnf t @=? Right r
