module TyCheckerSpec where

import AST
import Data.List.NonEmpty (NonEmpty (..))
import Test.QuickCheck.Instances.Natural ()
import TestPrelude
import TyCheck

test_inferType :: TestTree
test_inferType =
  testGroup
    "inferType"
    [ testProperty "Universe n : Universe (n + 1)" $
        \n -> inferType (Universe n) === Right (Universe (n + 1)),
      Magic `throwsError` UnannotatedMagic,
      (Magic `TyAnn` Magic) `infersType` Magic,
      Var "x" `throwsError` TypeVariableNotInScope "x",
      -- inconsistent universes
      pib "x" (Universe 0) (Universe 1) `infersType` Universe 2,
      pib "x" (Universe 1) (Universe 0) `infersType` Universe 2,
      -- parallel error handling
      let t = pib "x" (Magic `TyAnn` Magic) (Magic `TyAnn` Magic)
       in t `throwsError` MultipleTypeErrors (NonUniverseType (PiDomain t) Magic :| [NonUniverseType (PiCodomain t) Magic]),
      let t = lam "x" (Var "x") in t `throwsError` UnannotatedLambdaExpression t,
      let ty = pib "x'" (Universe 0) (Universe 0) in (lam "x" (Var "x") `TyAnn` ty) `infersType` ty
    ]
  where
    infersType t ty = testCase ("inferType $ " ++ show t) $ inferType t @?= Right ty
    throwsError t err = testCase ("inferType $ " ++ show t) $ inferType t @?= Left err

-- test_typeCheck =
--   testGroup
--     "typeCheck"
--     [ (lam "x" (Var "x") `App` Var "y") `hasType` Var "y",
--       -- note: bound normalizes variables internally so this kind of error
--       -- isn't actually very problematic in practice
--       (lam "x" (lam "y" (Var "x")) `App` Var "y") `hasType` lam "x" (Var "y"),
--       -- evaluation still happens under Pi and Lam
--       pib "z" (Universe 0) (lam "x" (Var "x") `App` Var "y") `hasType` pib "z" (Universe 0) (Var "y"),
--       lam "z" (lam "x" (Var "x") `App` Var "y") `hasType` lam "z" (Var "y")
--     ]
