module Evaluator where

import AST
import Bound
import ClassyPrelude
import Control.Monad.Error.Class

data RuntimeError
  = InvalidApplicationOf Term'
  | OtherRuntimeError Text
  deriving (Show, Eq, Generic)

-- | reduce a 'Term' to normal form using a lazy evaluation strategy
nf :: (MonadError RuntimeError m, Show a) => Term Text a -> m (Term Text a)
nf (Universe n) = pure $ Universe n
nf (Var x) = pure $ Var x
nf (TyAnn a _) = nf a
nf (Pi d s) = (Pi <$> nf d) <*> (toScope <$> nf (fromScope s))
nf (Lam s) = Lam . toScope <$> nf (fromScope s)
nf (App f a) = do
  f' <- whnf f
  case f' of
    Lam s -> nf $ instantiate1 a s
    Var x -> pure $ App (Var x) a
    _ -> throwError . InvalidApplicationOf $ tshow <$> f'

whnf :: (MonadError RuntimeError m, Show a) => Term Text a -> m (Term Text a)
whnf (App f a) = do
  f' <- whnf f
  case f' of
    Lam s -> whnf $ instantiate1 a s
    Var _ -> pure $ App f a
    _ -> throwError . InvalidApplicationOf $ tshow <$> f'
whnf t = pure t
