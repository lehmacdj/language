{-# LANGUAGE TemplateHaskell #-}

module AST where

import Bound
import Bound.Name
import ClassyPrelude
import Control.Lens
import qualified Control.Monad
import Data.Deriving
import Numeric.Natural

-- * definitions and instances

-- | Terms.
--
-- Enhancements to implement in the future:
-- * A type (or more generally a term) that is inferred. Syntactically expected
-- to be represented by an _. Type checking/decoration can do a best effort
-- attempt at guessing such holes, although it probably will be extremely
-- ineffective for terms.
-- * Dynamic type. A magic type that allows any term to be well typed.
-- * Undefined. An error value with type "forall a. a"
-- * Primitive types. Direct haskell implementations of things, instead of
-- encoding everything as Pi explicitly. This should also allow us to create
-- some slightly more interesting types like equality, which I'm not sure
-- it is possible to encode.
data Term n a
  = -- | The type of types. Universes should be interpreted to be cummulative.
    -- That is if x : Type i and i < j then x : Type j as well.
    Universe Natural
  | Var a
  | -- | Type annotation. Specifies that a term should have a specific type.
    -- When extending to include general comonadic annotations, they should
    -- subsume this constructor at least eventually if applicable. The type
    -- annotation uses the current scope, which means that behavior is similar
    -- to -XScopedTypeVariables.
    TyAnn (Term n a) (Term n a)
  | -- | Pi binders/forall requires an explicit domain to quantify over.
    Pi (Term n a) (Scope (Name n ()) (Term n) a)
  | Lam (Scope (Name n ()) (Term n) a)
  | App (Term n a) (Term n a)
  deriving (Functor, Foldable, Traversable, Generic)

instance Applicative (Term n) where
  pure = Var
  (<*>) = Control.Monad.ap

instance Monad (Term n) where
  Universe n >>= _ = Universe n
  Var v >>= f = f v
  TyAnn a b >>= f = TyAnn (a >>= f) (b >>= f)
  Pi d s >>= f = Pi (d >>= f) (s >>>= f)
  Lam s >>= f = Lam (s >>>= f)
  App a b >>= f = App (a >>= f) (b >>= f)

$(deriveEq1 ''Term)
$(deriveShow1 ''Term)
$(deriveEq ''Term)
$(deriveShow ''Term)
$(makePrisms ''Term)

-- * type aliases

-- | A simple term, using 'Text' to represent variables. This is what the AST
-- looks like when it is parsed.
type Term' = Term Text Text

-- * smart constructors
-- These bypass using abstract1Name explicitly, making constructing terms easier.

lam :: Eq n => n -> Term n n -> Term n n
lam n e = Lam (abstract1Name n e)

-- | pnemonic: pi-binder; name is not just pi to avoid conflict with pi :: Float
pib :: Eq n => n -> Term n n -> Term n n -> Term n n
pib n d e = Pi d (abstract1Name n e)
