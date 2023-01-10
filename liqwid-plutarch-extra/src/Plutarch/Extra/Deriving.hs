{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Extra.Deriving (
  FromPInner (..),
) where

import Data.Coerce (coerce)
import Data.Semigroup (sconcat, stimes)
import Plutarch.Unsafe (punsafeDowncast)

{- | Derivation helper to obtain 'Semigroup' and 'Monoid' instances for a type
 by way of its 'PInner' representation.

 = Important notes

 Do /not/ use this helper for any type @a@ where @'PInner' a ~ a@. This will
 loop the compiler.

 Furthermore, only use this derivation if 'Semigroup' and 'Monoid' methods
 cannot produce invalid values of the type being derived for. For example, if
 '<>' between 'PInner's can produce an invalid value of your type, deriving in
 this manner will allow such values to exist.

 @since 3.14.0
-}
newtype FromPInner (a :: S -> Type) (s :: S)
  = FromPInner (Term s a)

-- | @since 3.14.0
instance
  (PInnerSemigroup a (PInner a)) =>
  Semigroup (FromPInner a s)
  where
  {-# INLINEABLE (<>) #-}
  FromPInner t <> FromPInner t' = FromPInner . punsafeDowncast $ pto t <> pto t'
  {-# INLINEABLE stimes #-}
  stimes reps (FromPInner t) =
    FromPInner . punsafeDowncast . stimes reps . pto $ t
  {-# INLINEABLE sconcat #-}
  sconcat = FromPInner . punsafeDowncast . sconcat . fmap (pto . coerce)

-- | @since 3.14.0
instance
  (PInnerMonoid a (PInner a)) =>
  Monoid (FromPInner a s)
  where
  {-# INLINEABLE mempty #-}
  mempty = FromPInner . punsafeDowncast $ mempty

-- Helpers

-- Class synonym instances to get around the problem described in this issue
-- here: https://gitlab.haskell.org/ghc/ghc/-/issues/17959
--
-- More specifically, if we tried to write:
--
-- instance (forall s . Semigroup (Term s (PInner a))) => ...
--
-- GHC would reject this. However, using something like
--
-- instance (PInnerSemigroup a (PInner a)) => ...
--
-- works correctly.
class
  (b ~ PInner a, forall (s :: S). Semigroup (Term s b)) =>
  PInnerSemigroup a b

instance
  (b ~ PInner a, forall (s :: S). Semigroup (Term s b)) =>
  PInnerSemigroup a b

class
  (b ~ PInner a, forall (s :: S). Monoid (Term s b)) =>
  PInnerMonoid a b

instance
  (b ~ PInner a, forall (s :: S). Monoid (Term s b)) =>
  PInnerMonoid a b
