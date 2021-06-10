module TyCheck where

import AST
import Bound
import Control.Monad.Error.Class
import Data.List.NonEmpty (NonEmpty (..))
import Evaluator
import MyPrelude
import Numeric.Natural

data UniverseTypeCheckingContext
  = -- | domain of a pi type + the pi type
    PiDomain Term'
  | -- codomain of a pi type + the pi type
    PiCodomain Term'
  | -- | while checking that a term has a specific type e.g. via a TyAnn
    -- + tye term and type of the assertion
    TypeAssertion Term' Term'
  deriving (Show, Eq, Generic)

data TypeError
  = -- | Type variable with specified name was free / not in scope during type
    -- checking
    TypeVariableNotInScope Text
  | RuntimeErrorWhileEvaluatingType RuntimeError
  | -- | While type checking in some context  to be a
    -- universe type but was second term
    NonUniverseType UniverseTypeCheckingContext Term'
  | -- | While type checking pi type (first term) expected codomain to be a
    -- universe type but was second term
    NonUniverseCodomain Term' Term'
  | -- | First term appears in an application and is expected to have a pi type
    -- but is inferred as having second term
    ApplicationToTermWithoutFunctionType Term' Term'
  | UnannotatedLambdaExpression Term'
  | UnannotatedMagic
  | -- | Type mismatch while typechecking first term. Expected second term
    -- doesn't match inferred type third term
    TypeMismatch Term' Term' Term'
  | -- | Lambda (first term) has a type that is second term instead of a Pi type
    -- as expected
    LambdaWithNonPiType Term' Term'
  | OtherTypeError Term'
  | MultipleTypeErrors (NonEmpty TypeError)
  deriving (Show, Eq, Generic)

-- | Combine multiple type errors preserving the order of the type errors
instance Semigroup TypeError where
  MultipleTypeErrors es <> MultipleTypeErrors fs = MultipleTypeErrors (es <> fs)
  MultipleTypeErrors es <> f = MultipleTypeErrors (es <> singleton f)
  e <> MultipleTypeErrors fs = MultipleTypeErrors (e `cons` fs)
  e <> f = MultipleTypeErrors (e :| [f])

subsumeRuntimeError ::
  MonadError TypeError n =>
  (forall m. MonadError RuntimeError m => m a) ->
  n a
subsumeRuntimeError = subsumeError RuntimeErrorWhileEvaluatingType

assertHasUniverseType ::
  MonadError TypeError m =>
  UniverseTypeCheckingContext ->
  Term' ->
  m Natural
assertHasUniverseType context t = do
  tTy <- inferType t
  case tTy of
    Universe n -> pure n
    _ -> throwError $ NonUniverseType context tTy

-- | Given a term without a type compute its type + ensure that it is well typed
-- with that inferred type. Type inference may fail with an ambiguous type
-- error.
inferType ::
  (MonadError TypeError m) =>
  Term' ->
  m Term'
inferType = \case
  Universe n -> pure $ Universe (succ n)
  Magic -> throwError UnannotatedMagic
  Var v -> throwError $ TypeVariableNotInScope v
  TyAnn t ty -> typeCheck t ty >> pure ty
  Pi d s -> do
    let domainUniverseLevel = do
          dTy <- inferType d
          case dTy of
            Universe n -> pure n
            _ -> throwError $ NonUniverseType (PiDomain (Pi d s)) dTy
        codomainUniverseLevel = do
          d' <- subsumeRuntimeError $ nf d
          sTy <- inferType (instantiate1 (Magic `TyAnn` d') s)
          case sTy of
            Universe n -> pure n
            _ -> throwError $ NonUniverseType (PiCodomain (Pi d s)) sTy
    (domainUniverseLevel', codomainUniverseLevel') <-
      errorsParallelly domainUniverseLevel codomainUniverseLevel
    -- if both the domain and codomain are <= Universe i then we can type the
    -- function space as Universe i, by instead typing the domain and codomain
    -- each as Universe i
    pure $ Universe (max domainUniverseLevel' codomainUniverseLevel')
  Lam s -> throwError $ UnannotatedLambdaExpression (Lam s)
  App a b -> do
    aTy <- inferType a
    (domainTy, rangeTyScope) <- case aTy of
      Pi d s -> pure (d, s)
      _ -> throwError $ ApplicationToTermWithoutFunctionType (App a b) aTy
    typeCheck b domainTy
    pure $ instantiate1 (Magic `TyAnn` b) rangeTyScope

-- | Does the term (first arg) have the specified type (second arg).
typeCheck ::
  (MonadError TypeError m) =>
  Term' ->
  Term' ->
  m ()
typeCheck t ty = case t of
  Magic -> pure ()
  Lam s -> do
    _ <- assertHasUniverseType (TypeAssertion t ty) ty
    tyTy <- subsumeRuntimeError $ nf ty
    case tyTy of
      Pi d c -> typeCheck (instantiate1 (Magic `TyAnn` d) s) (instantiate1 (Magic `TyAnn` d) c)
      _ -> throwError $ LambdaWithNonPiType (Lam s) tyTy
  _ -> do
    let inferredTermTyNf = do
          tTy <- inferType t
          subsumeRuntimeError $ nf tTy
        wellTypedTyNf = do
          -- we don't care about the level of the universe when checking, just
          -- that there is one
          _ <- assertHasUniverseType (TypeAssertion t ty) ty
          subsumeRuntimeError $ nf ty
    (inferredTermTyNf', wellTypedTyNf') <-
      errorsParallelly inferredTermTyNf wellTypedTyNf
    unless (inferredTermTyNf' == wellTypedTyNf') $
      throwError $ TypeMismatch t ty inferredTermTyNf'
