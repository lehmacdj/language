{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MyPrelude
  ( module ClassyPrelude,
    module MyPrelude,
    HasCallStack,
    module Polysemy,
  )
where

import ClassyPrelude hiding (try)
import Control.Monad.Except
import GHC.Stack
import Polysemy
import Polysemy.Error

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

fromJustEx :: HasCallStack => Maybe a -> a
fromJustEx = \case
  Just x -> x
  Nothing -> error "expected Just but was Nothing"
