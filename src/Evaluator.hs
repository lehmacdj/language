module Evaluator where

import AST
import Bound
import Control.Comonad
import qualified Data.Map as Map
import MyPrelude
import Polysemy.Error
import Prettyprinter

data RuntimeError
  = InvalidApplicationOf Term'
  | ProjectionOfNonRecord Term'
  | ProjectionOfMissingRecordLabel Text Term'
  | OtherRuntimeError Text
  deriving (Show, Eq, Generic)

instance Pretty RuntimeError where
  pretty = viaShow

-- | reduce a 'Term' to normal form using a lazy evaluation strategy
nf ::
  forall r a.
  (Member (Error RuntimeError) r, Show a, HasCallStack) =>
  Term Text a ->
  Sem r (Term Text a)
nf (Universe n) = pure $ Universe n
nf Magic = pure Magic
nf Inferred = pure Inferred
nf (Var x) = pure $ Var x
nf (MetaVar x) = pure $ MetaVar x
nf (TyAnn a _) = nf a
nf (Pi d s) = (Pi <$> nf d) <*> (toScope <$> nf (fromScope s))
nf (Lam ty s) = Lam ty . toScope <$> nf (fromScope s)
nf (App f a) = do
  f' <- whnf f
  case f' of
    Lam _ s -> nf $ instantiate1 a s
    Var x -> pure $ App (Var x) a
    _ -> throw . InvalidApplicationOf $ tshow <$> f
nf (RecordTy m) = RecordTy <$> traverse nf m
nf (Record m) = Record <$> (traverse . #entry) (fmap toScope . nf . fromScope) m
nf (Project t l) = do
  t' <- whnf t
  m <- case t' of
    Record m -> pure m
    _ -> throw . ProjectionOfNonRecord $ tshow <$> t
  let makeSubstitution l' re =
        (view #index re, Project (Record m) l')
      recSubstitutions :: Map Int (Term Text a)
      recSubstitutions = mapFromList . toList $ Map.mapWithKey makeSubstitution m
      invariantFailure = error "index is impossible, map constructed from record"
      instantiateIndex :: Comonad w => w Int -> Term Text a
      instantiateIndex i = fromMaybe invariantFailure $ lookup (extract i) recSubstitutions
      tyErr = throw . ProjectionOfMissingRecordLabel l $ tshow <$> t
  maybe tyErr nf $
    instantiate instantiateIndex . view #entry <$> lookup l m

-- | Reduce to weak head normal form so that the basic shape of the data
-- is known. This only performs two kinds of evaluation:
-- * 'App' is reduced
-- * 'TyAnn' is removed
whnf ::
  (Member (Error RuntimeError) r, Show a) =>
  Term Text a ->
  Sem r (Term Text a)
whnf (TyAnn x _) = pure x
whnf (App f a) = do
  f' <- whnf f
  case f' of
    Lam _ s -> whnf $ instantiate1 a s
    Var _ -> pure $ App f a
    _ -> throw . InvalidApplicationOf $ tshow <$> f'
whnf t = pure t
