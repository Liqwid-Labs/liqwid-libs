module Plutarch.Extra.Monoid (
  PAll (..),
  pgetAll,
  PAny (..),
  pgetAny,
) where

import Control.Composition ((.*))
import Data.Function (on)
import Plutarch.Unsafe (punsafeCoerce)

-- | @since 1.3.0
newtype PAll (s :: S) = PAll (Term s PBool)
  deriving stock
    ( -- | @since 1.4.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.3.0
      PlutusType
    , -- | @since 1.3.0
      PIsData
    , -- | @since 1.3.0
      PEq
    , -- | @since 1.3.0
      PShow
    )

-- | @since 1.4.0
instance DerivePlutusType PAll where
  type DPTStrat _ = PlutusTypeNewtype

instance forall (s :: S). Semigroup (Term s PAll) where
  x <> y = pcon . PAll $ pto x #&& pto y

instance forall (s :: S). Monoid (Term s PAll) where
  mempty = pcon $ PAll $ pconstant True

pgetAll :: forall (s :: S). Term s (PAll :--> PBool)
pgetAll = plam punsafeCoerce

-- | @since 1.3.0
newtype PAny (s :: S) = PAny (Term s PBool)
  deriving stock
    ( -- | @since 1.4.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.3.0
      PlutusType
    , -- | @since 1.3.0
      PIsData
    , -- | @since 1.3.0
      PEq
    , -- | @since 1.3.0
      PShow
    )

-- | @since 1.4.0
instance DerivePlutusType PAny where
  type DPTStrat _ = PlutusTypeNewtype

pgetAny :: forall (s :: S). Term s (PAny :--> PBool)
pgetAny = plam punsafeCoerce

instance forall (s :: S). Semigroup (Term s PAny) where
  (<>) = pcon . PAny .* (#&&) `on` punsafeCoerce

instance forall (s :: S). Monoid (Term s PAny) where
  mempty = pcon $ PAny $ pconstant False
