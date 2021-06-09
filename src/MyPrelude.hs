module MyPrelude
  ( module ClassyPrelude,
    module MyPrelude,
  )
where

import ClassyPrelude
import Control.Monad.Except

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

-- | sequence two errors combining with a semigroup instance instead of
-- swallowing one, similarly to how validation operates but more generally
errorsParallelly ::
  (MonadError e m, Semigroup e) =>
  ExceptT e m a ->
  ExceptT e m b ->
  m (a, b)
errorsParallelly a b = do
  a' <- runExceptT a
  b' <- runExceptT b
  case (a', b') of
    (Left e, Left f) -> throwError (e <> f)
    (Left e, _) -> throwError e
    (_, Left f) -> throwError f
    (Right a'', Right b'') -> pure (a'', b'')

subsumeNothing ::
  MonadError e m =>
  e ->
  Maybe a ->
  m a
subsumeNothing e = maybe (throwError e) pure
