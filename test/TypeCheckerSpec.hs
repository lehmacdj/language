module TypeCheckerSpec where

import AST
import TestPrelude
import TyCheck

-- test_typeCheck :: TestGroup
-- test_typeCheck =
--   testGroup
--     "typeCheck"
--     [ testCase "base type" $ typeCheck [] [] Unit @?= Right UnitTy,
--       testCase "var type in env" $
--         typeCheck [("x", UnitTy)] [] (Var "x") @?= Right UnitTy,
--       testCase "var type not in env" $
--         typeCheck [] [] (Var "x") @?= Left "Variable \"x\" is not in scope",
--       testCase "func type" $
--         typeCheck [] [] (Lam "x" UnitTy Unit) @?= Right (FunTy UnitTy UnitTy),
--       testCase "func type extends env" $
--         typeCheck [("x", FunTy UnitTy UnitTy)] [] (Lam "x" UnitTy Unit)
--           @?= Right (FunTy UnitTy UnitTy),
--       testCase "forall" $
--         typeCheck [] [] (TyLam "a" Unit) @?= Right (Forall "a" UnitTy),
--       testCase "application" $
--         typeCheck [] [] (Lam "a" UnitTy Unit `App` Unit)
--           @?= Right UnitTy,
--       testCase "type application" $
--         typeCheck [] [] (TyLam "a" (Lam "x" (TyVar "a") (Var "x")) `TyApp` UnitTy)
--           @?= Right (FunTy UnitTy UnitTy),
--       testCase "acoherent type fails" $
--         typeCheck [] [] (Lam "x" (TyVar "a") (Var "x"))
--           @?= Left "TyVar (TyVariable \"a\") is not coherent"
--     ]
