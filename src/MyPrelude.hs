{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MyPrelude
  ( module ClassyPrelude,
    module MyPrelude,
    HasCallStack,
    module Polysemy,
    module Data.Generics.Labels,

    -- * lens re-exports
    view,
  )
where

import ClassyPrelude hiding (try)
import Control.Lens (Const (..), Getting, view)
import Control.Monad.Except
import Data.Generics.Labels
import GHC.Stack
import Polysemy
import Polysemy.Error
import Validation

subsumeError ::
  MonadError f n =>
  (e -> f) ->
  (forall m. MonadError e m => m a) ->
  n a
subsumeError f c = do
  result <- runExceptT c
  case result of
    Left e -> throwError $ f e
    Right r -> pure r

errorsParallelly ::
  (Member (Error e) r, Semigroup e) =>
  Sem r a ->
  Sem r b ->
  Sem r (a, b)
errorsParallelly a b = do
  a' <- try a
  b' <- try b
  case (a', b') of
    (Left e, Left f) -> throw (e <> f)
    (Left e, _) -> throw e
    (_, Left f) -> throw f
    (Right a'', Right b'') -> pure (a'', b'')

sequenceErrorsParallelly ::
  (Member (Error e) r, Semigroup e) =>
  [Sem r a] ->
  Sem r [a]
sequenceErrorsParallelly as =
  validation throw pure . traverse eitherToValidation =<< sequence (try <$> as)

fromJustEx :: HasCallStack => Maybe a -> a
fromJustEx = \case
  Just x -> x
  Nothing -> error "expected Just but was Nothing"

fromEitherVia :: Member (Error e') r => (e -> e') -> Either e a -> Sem r a
fromEitherVia f = \case
  Left e -> throw $ f e
  Right a -> pure a

-- | Copied from cabal codebase
toSetOf :: Getting (Set a) s a -> s -> Set a
toSetOf l s = getConst (l (Const . singleton) s)
