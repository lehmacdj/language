module TyCheck where

import AST
import Bound
import Data.List.NonEmpty (NonEmpty (..))
import Evaluator
import MyPrelude
import Numeric.Natural
import Polysemy.Error
import Prettyprinter

data UniverseTypeCheckingContext
  = -- | domain of a pi type + the pi type
    PiDomain Term'
  | -- codomain of a pi type + the pi type
    PiCodomain Term'
  | -- | while checking that a term has a specific type e.g. via a TyAnn
    -- + tye term and type of the assertion
    TypeAssertion Term' Term'
  | -- | while checking a label in the given record
    RecordTyWithLabel Text Term'
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
  | UninferredTerm
  | -- | Type mismatch while typechecking first term. Expected second term
    -- doesn't match inferred type third term
    TypeMismatch Term' Term' Term'
  | -- | Lambda (first term) has a type that is second term instead of a Pi type
    -- as expected
    LambdaWithNonPiType Term' Term'
  | OtherTypeError Term'
  | MultipleTypeErrors (NonEmpty TypeError)
  | -- | First term appears in a projection and is expected to have a record
    -- type but has the second term as its type.
    ProjectionOfNonRecordTy Term' Term'
  | -- | First term expected to have a type (second term) containing the label
    -- (third arg).
    ProjectionOfMissingRecordLabelTy Term' Term' Text
  deriving (Show, Eq, Generic)

instance Pretty TypeError where
  -- TODO: improve instance
  pretty = viaShow

-- | Combine multiple type errors preserving the order of the type errors
instance Semigroup TypeError where
  MultipleTypeErrors es <> MultipleTypeErrors fs = MultipleTypeErrors (es <> fs)
  MultipleTypeErrors es <> f = MultipleTypeErrors (es <> singleton f)
  e <> MultipleTypeErrors fs = MultipleTypeErrors (e `cons` fs)
  e <> f = MultipleTypeErrors (e :| [f])

subsumeRuntimeError ::
  Member (Error TypeError) r =>
  Sem (Error RuntimeError : r) a ->
  Sem r a
subsumeRuntimeError = mapError RuntimeErrorWhileEvaluatingType

assertHasUniverseType ::
  Member (Error TypeError) r =>
  UniverseTypeCheckingContext ->
  Term' ->
  Sem r Natural
assertHasUniverseType context t = do
  tTy <- inferType t
  case tTy of
    Universe n -> pure n
    _ -> throw $ NonUniverseType context tTy

-- | Given a term without a type compute its type + ensure that it is well typed
-- with that inferred type. Type inference may fail with an ambiguous type
-- error.
inferType ::
  Member (Error TypeError) r =>
  Term' ->
  Sem r Term'
inferType = \case
  Universe n -> pure $ Universe (succ n)
  Magic -> throw UnannotatedMagic
  Inferred -> throw UninferredTerm
  Var v -> throw $ TypeVariableNotInScope v
  TyAnn t ty -> typeCheck t ty >> pure ty
  Pi d s -> do
    -- unfortunately we can't type check the domain and codomain in parallel
    -- because type checking the codomain requires reducing the domain to
    -- normal form first. Note: we might be able to solve this by not reducing
    -- to normal form first, but this might have performance implications.
    domainUniverseLevel <- assertHasUniverseType (PiDomain (Pi d s)) d
    d' <- subsumeRuntimeError $ nf d
    codomainUniverseLevel <-
      assertHasUniverseType (PiCodomain (Pi d s)) (instantiate1 (Magic `TyAnn` d') s)
    -- if both the domain and codomain are <= Universe i then we can type the
    -- function space as Universe i, by instead typing the domain and codomain
    -- each as Universe i
    pure $ Universe (max domainUniverseLevel codomainUniverseLevel)
  -- TODO: take advantage of codomain ty
  -- note: to do this it will probably require switching to a more traditional
  -- type checking algorithm where we have a context because otherwise it
  -- doesn't seem possible to reconstruct the most general type that is roughly
  -- @pib x (nf ty) (inferType s)@. However, this will require unwrapping
  -- the scopes all the way down, and then reconstructing them. + it will be
  -- necessary to generate somewhat meaningful names for them. All in all,
  -- this seems kind of hard.
  Lam ty s -> throw $ UnannotatedLambdaExpression (Lam ty s)
  App a b -> do
    aTy <- inferType a
    (domainTy, rangeTyScope) <- case aTy of
      Pi d s -> pure (d, s)
      _ -> throw $ ApplicationToTermWithoutFunctionType (App a b) aTy
    typeCheck b domainTy
    pure $ instantiate1 (Magic `TyAnn` b) rangeTyScope
  RecordTy m -> do
    let assertHasUniverseType' (l, t) =
          assertHasUniverseType (RecordTyWithLabel l (RecordTy m)) t
    levels <- sequenceErrorsParallelly (assertHasUniverseType' <$> mapToList m)
    let level = maximum (0 `ncons` levels)
    pure $ Universe level
  Record m -> pure $ RecordTy (view #entryType <$> m)
  Project t l -> do
    tTy <- inferType t
    case tTy of
      RecordTy m -> note (ProjectionOfMissingRecordLabelTy t tTy l) $ lookup l m
      _ -> throw $ ProjectionOfNonRecordTy t tTy

-- | Does the term (first arg) have the specified type (second arg).
typeCheck ::
  Member (Error TypeError) r =>
  Term' ->
  Term' ->
  Sem r ()
typeCheck t ty = case t of
  -- Magic is allowed to have any syntactically valid type, even if it isn't
  -- a valid type for other purposes
  Magic -> pure ()
  Lam cdty s -> do
    _ <- assertHasUniverseType (TypeAssertion t ty) ty
    tyTy <- subsumeRuntimeError $ nf ty
    case tyTy of
      Pi d c -> typeCheck (instantiate1 (Magic `TyAnn` d) s) (instantiate1 (Magic `TyAnn` d) c)
      _ -> throw $ LambdaWithNonPiType (Lam cdty s) tyTy
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
      throw $ TypeMismatch t ty inferredTermTyNf'
