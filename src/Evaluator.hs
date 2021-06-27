module Evaluator where

import AST
import Bound
import MyPrelude
import Polysemy.Error
import Prettyprinter

data RuntimeError
  = InvalidApplicationOf Term'
  | OtherRuntimeError Text
  deriving (Show, Eq, Generic)

instance Pretty RuntimeError where
  pretty = viaShow

-- | reduce a 'Term' to normal form using a lazy evaluation strategy
nf ::
  (Member (Error RuntimeError) r, Show a) =>
  Term Text a ->
  Sem r (Term Text a)
nf (Universe n) = pure $ Universe n
nf Magic = pure Magic
nf (Var x) = pure $ Var x
nf (TyAnn a _) = nf a
nf (Pi d s) = (Pi <$> nf d) <*> (toScope <$> nf (fromScope s))
nf (Lam s) = Lam . toScope <$> nf (fromScope s)
nf (App f a) = do
  f' <- whnf f
  case f' of
    Lam s -> nf $ instantiate1 a s
    Var x -> pure $ App (Var x) a
    _ -> throw . InvalidApplicationOf $ tshow <$> f'

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
    Lam s -> whnf $ instantiate1 a s
    Var _ -> pure $ App f a
    _ -> throw . InvalidApplicationOf $ tshow <$> f'
whnf t = pure t
