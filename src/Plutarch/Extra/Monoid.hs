{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.Monoid (
    PAll (..),
    pgetAll,
    PAny (..),
    pgetAny,
) where

import Control.Composition ((.*))
import Data.Function (on)
import Plutarch (DerivePNewtype (..), PCon (pcon), PlutusType (..), S, Term, plam, type (:-->))
import Plutarch.Bool (PBool (..), PEq, (#&&))
import Plutarch.Builtin (PIsData)
import Plutarch.Lift (pconstant)
import Plutarch.Show (PShow)
import Plutarch.Unsafe (punsafeCoerce)

-- | @since 1.3.0
newtype PAll (s :: S) = PAll (Term s PBool)
    deriving
        ( -- | @since 1.3.0
          PlutusType
        , -- | @since 1.3.0
          PIsData
        , -- | @since 1.3.0
          PEq
        , -- | @since 1.3.0
          PShow
        )
        via (DerivePNewtype PAll PBool)

instance forall (s :: S). Semigroup (Term s PAll) where
    (<>) = pcon . PAll .* (#&&) `on` punsafeCoerce

instance forall (s :: S). Monoid (Term s PAll) where
    mempty = pcon $ PAll $ pconstant True

pgetAll :: forall (s :: S). Term s (PAll :--> PBool)
pgetAll = plam punsafeCoerce

-- | @since 1.3.0
newtype PAny (s :: S) = PAny (Term s PBool)
    deriving
        ( -- | @since 1.3.0
          PlutusType
        , -- | @since 1.3.0
          PIsData
        , -- | @since 1.3.0
          PEq
        , -- | @since 1.3.0
          PShow
        )
        via (DerivePNewtype PAny PBool)

pgetAny :: forall (s :: S). Term s (PAny :--> PBool)
pgetAny = plam punsafeCoerce

instance forall (s :: S). Semigroup (Term s PAny) where
    (<>) = pcon . PAny .* (#&&) `on` punsafeCoerce

instance forall (s :: S). Monoid (Term s PAny) where
    mempty = pcon $ PAny $ pconstant False
