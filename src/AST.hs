{-# LANGUAGE TemplateHaskell #-}

module AST where

import Bound
import Bound.Name
import ClassyPrelude
import qualified Control.Monad
import Data.Deriving
import Data.Functor.Classes
import GHC.Generics
import Numeric.Natural

-- * definitions and instances

data BaseType = UnitTy
  deriving (Show, Eq, Generic)

data DataConstructor = Unit
  deriving (Show, Eq, Generic)

data Term n a
  = BaseType BaseType
  | Constructor DataConstructor
  | Universe Natural
  | Var a
  | Pi (Scope (Name n ()) (Term n) a)
  | Lam (Scope (Name n ()) (Term n) a)
  | App (Term n a) (Term n a)
  deriving (Functor, Foldable, Traversable, Generic)

instance Applicative (Term n) where
  pure = Var
  (<*>) = Control.Monad.ap

instance Monad (Term n) where
  BaseType bt >>= _ = BaseType bt
  Constructor dc >>= _ = Constructor dc
  Universe n >>= _ = Universe n
  Var v >>= f = f v
  Pi s >>= f = Pi (s >>>= f)
  Lam s >>= f = Lam (s >>>= f)
  App a b >>= f = App (a >>= f) (b >>= f)

$(deriveEq1 ''Term)
$(deriveShow1 ''Term)
$(deriveEq ''Term)
$(deriveShow ''Term)

-- * type aliases

-- | A simple term, using 'Text' to represent variables.
type Term' a = Term Text a

-- * smart constructors
-- These bypass using abstract1Name explicitly, making constructing terms easier.

lam :: Eq n => n -> Term n n -> Term n n
lam n e = Lam (abstract1Name n e)

-- | pnemonic: pi-binder; name is not just pi to avoid conflict with pi :: Float
pib :: Eq n => n -> Term n n -> Term n n
pib n e = Pi (abstract1Name n e)
