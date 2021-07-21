module TyCheck
  ( inferType,
    typeCheck,

    -- * Error Types
    TypeError (..),
    UniverseTypeCheckingContext (..),
  )
where

import AST
import Bound
import Control.Comonad
import Data.List.NonEmpty (NonEmpty (..))
import Evaluator
import MyPrelude
import Numeric.Natural
import Polysemy.Error
import Prettyprinter

data UniverseTypeCheckingContext
  = -- | domain of a lambda binder + the lambda expression
    LambdaDomain Term'
  | -- | domain of a pi type + the pi type
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
  | MetaVarLeftInTermDuringTypeChecking
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
  (Show a, Eq a, Member (Error TypeError) r) =>
  Ctx r a ->
  UniverseTypeCheckingContext ->
  Term Text a ->
  Sem r Natural
assertHasUniverseType ctx context t = do
  tTy <- inferTypeCtx ctx t
  case tTy of
    Universe n -> pure n
    _ -> throw . NonUniverseType context $ tshow <$> tTy

type Ctx r a = a -> Sem r (Term Text a)

emptyCtx :: (Member (Error TypeError) r, Eq a, Show a) => Ctx r a
emptyCtx v = throw . TypeVariableNotInScope $ tshow v

extendCtxUnit ::
  (Comonad w, Member (Error TypeError) r) => Term Text a -> Ctx r a -> Ctx r (Var (w ()) a)
extendCtxUnit ty cxt = \case
  B (extract -> ()) -> pure (F <$> ty)
  F a -> (F <$>) <$> cxt a

extendCtxInt ::
  (Comonad w, HasCallStack, Member (Error TypeError) r) =>
  (Int -> Term Text a) ->
  Ctx r a ->
  Ctx r (Var (w Int) a)
extendCtxInt f ctx = \case
  B n -> pure $ F <$> f (extract n)
  F a -> (F <$>) <$> ctx a

-- | Given a term without a type compute its type + ensure that it is well typed
-- with that inferred type. Type inference may fail with an ambiguous type
-- error.
inferType ::
  (Show a, Eq a, Member (Error TypeError) r) =>
  Term Text a ->
  Sem r (Term Text a)
inferType = inferTypeCtx emptyCtx

inferTypeCtx ::
  forall a r.
  (Show a, Eq a, Member (Error TypeError) r) =>
  Ctx r a ->
  Term Text a ->
  Sem r (Term Text a)
inferTypeCtx ctx = \case
  Universe n -> pure $ Universe (succ n)
  Magic -> throw UnannotatedMagic
  Inferred -> throw UninferredTerm
  Var v -> ctx v
  MetaVar _ -> throw MetaVarLeftInTermDuringTypeChecking
  TyAnn t ty -> typeCheckCtx ctx t ty >> pure ty
  Pi d s -> do
    -- unfortunately we can't type check the domain and codomain in parallel
    -- because type checking the codomain requires reducing the domain to
    -- normal form first. Note: we might be able to solve this by not reducing
    -- to normal form first, but this might have performance implications.
    domainUniverseLevel <- assertHasUniverseType ctx (PiDomain (tshow <$> Pi d s)) d
    d' <- subsumeRuntimeError $ nf d
    codomainUniverseLevel <-
      assertHasUniverseType
        (extendCtxUnit d' ctx)
        (PiCodomain (tshow <$> Pi d s))
        (fromScope s)
    -- if both the domain and codomain are <= Universe i then we can type the
    -- function space as Universe i, by instead typing the domain and codomain
    -- each as Universe i
    pure $ Universe (max domainUniverseLevel codomainUniverseLevel)
  Lam ty s -> do
    void $ assertHasUniverseType ctx (LambdaDomain (tshow <$> Lam ty s)) ty
    ty' <- subsumeRuntimeError . nf $ ty
    Pi ty' . toScope <$> inferTypeCtx (extendCtxUnit ty' ctx) (fromScope s)
  App a b -> do
    aTy <- inferType a
    (domainTy, rangeTyScope) <- case aTy of
      Pi d s -> pure (d, s)
      _ -> throw $ ApplicationToTermWithoutFunctionType (tshow <$> App a b) (tshow <$> aTy)
    typeCheckCtx ctx b domainTy
    subsumeRuntimeError . nf $ instantiate1 b rangeTyScope
  RecordTy m -> do
    let assertHasUniverseType' (l, t) =
          assertHasUniverseType ctx (RecordTyWithLabel l (tshow <$> RecordTy m)) t
    levels <- sequenceErrorsParallelly (assertHasUniverseType' <$> mapToList m)
    let level = maximum (0 `ncons` levels)
    pure $ Universe level
  Record m -> do
    let indexMap :: Map Int (Term Text a)
        indexMap = mapFromList $ (view #index &&& view #entryType) <$> toList m
        intToTy =
          fromMaybe (error "impossible index for bound variable")
            . flip lookup indexMap
        newCtx = extendCtxInt intToTy ctx
    void . sequenceErrorsParallelly $
      uncurry (typeCheckCtx newCtx)
        . ((fromScope . view #entry) &&& (fmap F . view #entryType))
        <$> toList m
    pure $ RecordTy (view #entryType <$> m)
  Project t l -> do
    tTy <- inferType t
    case tTy of
      RecordTy m ->
        note (ProjectionOfMissingRecordLabelTy (tshow <$> t) (tshow <$> tTy) l) $
          lookup l m
      _ -> throw $ ProjectionOfNonRecordTy (tshow <$> t) (tshow <$> tTy)

-- | Does the term (first arg) have the specified type (second arg).
typeCheck ::
  (Show a, Eq a, Member (Error TypeError) r) =>
  Term Text a ->
  Term Text a ->
  Sem r ()
typeCheck = typeCheckCtx emptyCtx

typeCheckCtx ::
  (Show a, Eq a, Member (Error TypeError) r) =>
  Ctx r a ->
  Term Text a ->
  Term Text a ->
  Sem r ()
typeCheckCtx ctx t ty = case t of
  -- Magic is allowed to have any syntactically valid type, even if it isn't
  -- a valid type for other purposes
  Magic -> pure ()
  -- TODO: get rid of this special case
  Lam cdty s -> do
    _ <- assertHasUniverseType ctx (TypeAssertion (tshow <$> t) (tshow <$> ty)) ty
    tyTy <- subsumeRuntimeError $ nf ty
    case tyTy of
      Pi d c -> typeCheck (instantiate1 (Magic `TyAnn` d) s) (instantiate1 (Magic `TyAnn` d) c)
      _ -> throw $ LambdaWithNonPiType (tshow <$> Lam cdty s) (tshow <$> tyTy)
  _ -> do
    let inferredTermTyNf = do
          tTy <- inferTypeCtx ctx t
          subsumeRuntimeError $ nf tTy
        wellTypedTyNf = do
          -- we don't care about the level of the universe when checking, just
          -- that there is one
          _ <- assertHasUniverseType ctx (TypeAssertion (tshow <$> t) (tshow <$> ty)) ty
          subsumeRuntimeError $ nf ty
    (inferredTermTyNf', wellTypedTyNf') <-
      errorsParallelly inferredTermTyNf wellTypedTyNf
    unless (inferredTermTyNf' == wellTypedTyNf') $
      throw $ TypeMismatch (tshow <$> t) (tshow <$> ty) (tshow <$> inferredTermTyNf')
