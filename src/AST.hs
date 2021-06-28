{-# LANGUAGE TemplateHaskell #-}

module AST where

import Bound
import Bound.Name
import Control.Lens
import qualified Control.Monad
import Data.Deriving
import qualified Data.Map as Map
import qualified Data.Set as Set
import MyPrelude
import Numeric.Natural

-- * definitions and instances

-- | To be able to have variables that are unnamed (e.g. for cases where there
-- must be no captures of the variable, i.e. when translating a -> b to a pi
-- binder), but still have name data for variables that are bound (see:
-- 'Bound.Name' we need to be able to not store a name with a bound termala
-- 'Name' but want it to also be possible for it to be missing.
type BoundName n = Name (Maybe n) ()

type BoundLabel n = Name (Maybe n) Int

abstract1BoundName :: (Monad f, Eq a) => a -> f a -> Scope (BoundName a) f a
abstract1BoundName n = abstract (\x -> if x == n then Just (Name (Just n) ()) else Nothing)

-- | Each record entry
-- The Int bindings start at 0 and count up for all of the elements in the
-- Map. Thus the number of bindings in the map is @length m@ and the largest
-- binding is @length m - 1@.
-- Each term is labeled with its type as well. This is necessary due to the
-- recursiveness of the bindings.
data RecordEntry n a = RecordEntry
  { index :: Int,
    entryType :: Term n a,
    entry :: Scope (BoundLabel n) (Term n) a
  }
  deriving (Functor, Foldable, Traversable, Generic)

-- | Terms.
--
-- Enhancements to implement in the future:
-- * A type (or more generally a term) that is inferred. Syntactically expected
-- to be represented by an _. Type checking/decoration can do a best effort
-- attempt at guessing such holes, although it probably will be extremely
-- ineffective for terms.
-- * Dynamic type. A magic type that allows any term to be well typed.
-- * Primitive types. Direct haskell implementations of things, instead of
-- encoding everything as Pi explicitly. This should also allow us to create
-- some slightly more interesting types like equality, which I'm not sure
-- it is possible to encode.
-- * Algebraic data types / homotopy types / records. Some kinds of stronger
-- more built in types.
-- * Dependent records. We probably want to just upgrade the existing records.
data Term n a
  = -- | The type of types. Universes should be interpreted to be cummulative.
    -- That is if x : Type i and i < j then x : Type j as well.
    Universe Natural
  | -- | A term that has type forall a. a; obviously this is unsound, but it
    -- is useful for introducing axioms + some other special purpose uses.
    Magic
  | -- | A term that is inferred by the elaborator. Sometimes this will fail
    -- and lead to a type error.
    Inferred
  | Var a
  | -- | Type annotation. Specifies that a term should have a specific type.
    -- When extending to include general comonadic annotations, they should
    -- subsume this constructor at least eventually if applicable. The type
    -- annotation uses the current scope, which means that behavior is similar
    -- to -XScopedTypeVariables.
    TyAnn (Term n a) (Term n a)
  | -- | Pi binders/forall requires an explicit domain to quantify over.
    Pi (Term n a) (Scope (BoundName n) (Term n) a)
  | Lam (Term n a) (Scope (BoundName n) (Term n) a)
  | App (Term n a) (Term n a)
  | -- | Type of non-dependent record with arbitrary labels. Each label is
    -- associated with a type stored in that label.
    RecordTy (Map n (Term n a))
  | -- | Non-dependent record with arbitrary labels. Records bind labels to
    -- terms. The bindings may be recursive. Yes this does lead to soundness
    -- issues when viewed as a logic. Don't use this kind of record if that
    -- is important to you.
    Record (Map n (RecordEntry n a))
  | -- | Project a field of a record.
    Project (Term n a) n
  deriving (Functor, Foldable, Traversable, Generic)

instance Applicative (Term n) where
  pure = Var
  (<*>) = Control.Monad.ap

instance Monad (Term n) where
  Universe n >>= _ = Universe n
  Magic >>= _ = Magic
  Inferred >>= _ = Inferred
  Var v >>= f = f v
  TyAnn a b >>= f = TyAnn (a >>= f) (b >>= f)
  Pi d s >>= f = Pi (d >>= f) (s >>>= f)
  Lam ty s >>= f = Lam (ty >>= f) (s >>>= f)
  App a b >>= f = App (a >>= f) (b >>= f)
  RecordTy m >>= f = RecordTy (fmap (>>= f) m)
  Record m >>= f = Record (fmap bindEntry m)
    where
      bindEntry (RecordEntry i ty s) = RecordEntry i (ty >>= f) (s >>>= f)
  Project t n >>= f = Project (t >>= f) n

$(deriveEq1 ''RecordEntry)
$(deriveShow1 ''RecordEntry)
$(deriveOrd1 ''RecordEntry)
$(deriveEq ''RecordEntry)
$(deriveOrd ''RecordEntry)
$(deriveShow ''RecordEntry)

$(deriveEq1 ''Term)
$(deriveShow1 ''Term)
$(deriveOrd1 ''Term)
$(deriveEq ''Term)
$(deriveOrd ''Term)
$(deriveShow ''Term)

-- | A simple term, using 'Text' to represent variables. This is what the AST
-- looks like when it is parsed.
type Term' = Term Text Text

-- * smart constructors
-- These bypass using abstract1Name explicitly, making constructing terms easier.

lam :: Eq n => n -> Term n n -> Term n n
lam n e = Lam Inferred (abstract1BoundName n e)

-- | pnemonic: pi-binder; name is not just pi to avoid conflict with pi :: Float
pib :: Eq n => n -> Term n n -> Term n n -> Term n n
pib n d e = Pi d (abstract1BoundName n e)

-- | for constructing non dependent function types. Use of fromJustEx is safe
-- because abstractName1 removes all instances of Nothing from the input and
-- because there was no nothing in the first place because the term was produced
-- by mapping with Just.
arrow :: Eq n => Term n n -> Term n n -> Term n n
arrow d e = Pi d (fmap fromJustEx (abstract1Name Nothing (fmap Just e)))

hasNoDuplicateLabels :: Ord n => [(n, a)] -> Bool
hasNoDuplicateLabels bindings =
  length (toSetOf (folded . _1) bindings) == length bindings

makeSmartRecordMaker ::
  Ord n => ([(n, a)] -> Term n n) -> [(n, a)] -> Maybe (Term n n)
makeSmartRecordMaker f bindings
  | hasNoDuplicateLabels bindings = Nothing
  | otherwise = Just $ f bindings

-- | Create a record; bindings must be non duplicate. Otherwise returns Nothing
record :: Ord n => [(n, Term n n)] -> Maybe (Term n n)
record = makeSmartRecordMaker record'

-- | Create a max record type; bindings must be non duplicate. Otherwise returns Nothing
recordTy :: Ord n => [(n, Term n n)] -> Maybe (Term n n)
recordTy = makeSmartRecordMaker recordTy'

-- | Used exclusively in the definition of record' as a glorified tuple
data PreRecordEntry n e = PreRecordEntry
  { index :: Int,
    entryType :: Term n n,
    entry :: e
  }
  deriving (Generic)

-- | Create a record; for duplicate bindings the last binding is used.
-- This function is less safe, and should tend to only be used for constructing
-- test data or in situations where we statically know that the record
-- values are well formed.
record' :: Ord n => [(n, Term n n)] -> Term n n
record' bindings =
  Record
    . fmap toRecordEntry
    . over (mapped . #entry) (abstract boundVarFor)
    $ bindings'
  where
    mkPreRecordEntry (l, t) i = (l, PreRecordEntry i Inferred t)
    toRecordEntry (PreRecordEntry i ty t) = RecordEntry i ty t
    bindings' = mapFromList $ zipWith mkPreRecordEntry bindings [0 ..]
    boundVarFor x = Name (Just x) . view #index <$> lookup x bindings'

-- | Create a record type; for duplicate bindings the last binding is used.
-- This function is less safe, and should tend to only be used for constructing
-- test data or in situations where we statically know that the record
-- values are well formed.
recordTy' :: Ord n => [(n, Term n n)] -> Term n n
recordTy' = RecordTy . mapFromList
