module TyCheckerSpec where

import AST
import Data.List.NonEmpty (NonEmpty (..))
import Polysemy.Error
import Test.QuickCheck.Instances.Natural ()
import TestPrelude
import TyCheck

runE :: Sem '[Error e] a -> Either e a
runE = run . runError

test_inferType :: TestTree
test_inferType =
  testGroup
    "inferType"
    [ testProperty "Universe n : Universe (n + 1)" $
        \n -> runE (inferType (Universe n)) === Right (Universe (n + 1)),
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
      let ty = pib "x'" (Universe 0) (Universe 0) in (lam "x" (Var "x") `TyAnn` ty) `infersType` ty,
      ((Magic `TyAnn` pib "x" (Universe 0) (Universe 0)) `App` (Magic `TyAnn` Universe 0)) `infersType` Universe 0,
      let t = Universe 0 `App` Universe 0 in t `throwsError` ApplicationToTermWithoutFunctionType t (Universe 1)
    ]
  where
    infersType t ty = testCase ("inferType $ " ++ show t) $ runE (inferType t) @?= Right ty
    throwsError t err = testCase ("inferType $ " ++ show t) $ runE (inferType t) @?= Left err

test_typeCheck :: TestTree
test_typeCheck =
  testGroup
    "typeCheck"
    [ Magic `hasType` Universe 91,
      Magic `hasType` pib "x" (Universe 3) (Universe 2),
      Magic `hasType` Magic,
      -- currently allowing magic to have a totally arbitrary type,
      -- this behavior might be undesireable at some point, in which case
      -- removing these tests would be completely fine
      Magic `hasType` lam "x" (Var "x"),
      Magic `hasType` (Universe 0 `TyAnn` Universe 0),
      lam "x" (Var "x") `hasType` pib "x" (Universe 0) (Universe 0),
      let t = lam "x" (Var "x") in throwsError t (Magic `TyAnn` Universe 0) $ LambdaWithNonPiType t Magic,
      throwsError (Var "x") (Universe 0) $ TypeVariableNotInScope "x",
      (Magic `TyAnn` Magic) `hasType` (Magic `TyAnn` Universe 0),
      let t = (Magic `TyAnn` Universe 1) in throwsError t (Universe 0) $ TypeMismatch t (Universe 0) (Universe 1)
    ]
  where
    hasType t ty = testCase (show t ++ " : " ++ show ty) $ runE (typeCheck t ty) @?= Right ()
    throwsError t ty err = testCase (show t ++ " : " ++ show ty) $ runE (typeCheck t ty) @?= Left err
