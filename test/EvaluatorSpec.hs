module EvaluatorSpec where

import AST
import Evaluator
import Polysemy.Error
import Test.QuickCheck.Instances.Natural ()
import TestPrelude

runE :: Sem '[Error e] a -> Either e a
runE = run . runError

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
      lam "z" (lam "x" (Var "x") `App` Var "y") `hasNf` lam "z" (Var "y"),
      Magic `hasNf` Magic,
      testProperty "Universe n is in normal form" $ \n ->
        runE (nf (Universe n)) === Right (Universe n :: Term')
    ]
  where
    hasNf :: Term' -> Term' -> TestTree
    t `hasNf` r = testCase ("nf $ " ++ show t) $ runE (nf t) @?= Right r

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
      lam "z" (lam "x" (Var "x") `App` Var "y") `hasWhnf` lam "z" (lam "x" (Var "x") `App` Var "y"),
      Magic `hasWhnf` Magic,
      testProperty "Universe n is in weak-head normal form" $ \n ->
        runE (whnf (Universe n)) === Right (Universe n :: Term')
    ]
  where
    hasWhnf :: Term' -> Term' -> TestTree
    t `hasWhnf` r = testCase ("whnf $ " ++ show t) $ runE (whnf t) @?= Right r
