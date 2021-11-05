module MyPrelude
  ( -- * At its core the prelude is ClassyPrelude + some extras
    module ClassyPrelude,

    -- * Misc functions
    bug,
    errorsParallelly,
    fromEitherVia,
    fromJustEx,
    mapError,
    sequenceErrorsParallelly,

    -- * misc re-exports
    module X,
  )
where

import ClassyPrelude hiding (Handler, error, try)
import qualified ClassyPrelude
import Control.Effect as X
import Control.Effect.Error
import Control.Lens as X (Fold, Getter, Iso, Iso', Lens, Lens', Prism, Prism', Setter, Traversal, Traversal', folding, iso, lens, preview, prism, review, set, to, view, (.~), (^.), (^?))
import Control.Monad.Except as X (ExceptT)
import Data.Generics.Labels as X
import Data.Void as X (Void)
import GHC.Stack as X (HasCallStack)
import Validation

errorsParallelly ::
  (Eff (Error e) m, Semigroup e) =>
  m a ->
  m b ->
  m (a, b)
errorsParallelly a b = do
  a' <- try a
  b' <- try b
  case (a', b') of
    (Left e, Left f) -> throw (e <> f)
    (Left e, _) -> throw e
    (_, Left f) -> throw f
    (Right a'', Right b'') -> pure (a'', b'')

sequenceErrorsParallelly ::
  (Eff (Error e) m, Semigroup e) =>
  [m a] ->
  m [a]
sequenceErrorsParallelly as =
  validation throw pure . traverse eitherToValidation =<< sequence (try <$> as)

mapError ::
  Eff (Error errBig) m =>
  Prism' errBig errSmall ->
  InterpretErrorC errSmall m a ->
  m a
mapError p = errorToError (review p) (preview p)

fromJustEx :: HasCallStack => Maybe a -> a
fromJustEx = \case
  Just x -> x
  Nothing -> bug "expected Just but was Nothing"

fromEitherVia :: Eff (Error e') m => (e -> e') -> Either e a -> m a
fromEitherVia f = \case
  Left e -> throw $ f e
  Right a -> pure a

bug :: HasCallStack => Text -> a
bug = ClassyPrelude.error . unpack
