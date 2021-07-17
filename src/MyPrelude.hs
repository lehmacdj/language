module MyPrelude
  ( -- * At its core the prelude is ClassyPrelude + some extras
    module ClassyPrelude,

    -- * Misc functions
    errorsParallelly,
    sequenceErrorsParallelly,
    fromJustEx,
    fromEitherVia,
    toSetOf,

    -- * misc re-exports
    module X,
  )
where

import ClassyPrelude hiding (try)
import Control.Lens (Const (..), Getting)
import Control.Lens as X (view)
import Data.Generics.Labels as X
import Data.Void as X (Void)
import GHC.Stack as X (HasCallStack)
import Polysemy as X
import Polysemy.Error
import Validation

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
