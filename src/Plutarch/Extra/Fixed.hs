{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.Fixed (
  PFixed (..),
  DivideSemigroup (..),
  DivideMonoid (..),
  fixedToAdaValue,
  fromPInteger,
  toPInteger,
) where

import Control.Composition (on, (.*))
import Data.Bifunctor (first)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, Nat, natVal)
import Plutarch.Api.V1 (AmountGuarantees (NonZero), PValue)
import qualified Plutarch.Api.V1.Value as Value
import Plutarch.Api.V2 (KeyGuarantees (Sorted))
import Plutarch.Extra.Function (pflip)
import Plutarch.Num (PNum (pfromInteger, (#*)))
import qualified Plutarch.Numeric.Additive as A (
  AdditiveMonoid (zero),
  AdditiveSemigroup ((+)),
 )
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)

{- | Fixed width decimal. Denominator will be given through typelit `unit`.
 This would be used for representing Ada value with some Lovelace changes.

 @since 3.12.0
-}
{-# DEPRECATED PFixed "Use PFixedDecimal instead" #-}

newtype PFixed (unit :: Nat) (s :: S)
  = PFixed (Term s PInteger)
  deriving stock
    ( -- | @since 3.12.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.12.0
      PlutusType
    , -- | @since 3.12.0
      PIsData
    , -- | @since 3.12.0
      PEq
    , -- | @since 3.12.0
      PPartialOrd
    , -- | @since 3.12.0
      POrd
    , -- | @since 3.12.0
      PShow
    )

-- | @since 3.12.0
instance DerivePlutusType (PFixed a) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.12.0
instance KnownNat u => PNum (PFixed u) where
  (#*) =
    (pcon . PFixed)
      .* (pflip # pdiv # pconstant (natVal (Proxy @u)) #)
      .* (#*)
      `on` punsafeCoerce
  pfromInteger = pcon . PFixed . (* pconstant (natVal (Proxy @u))) . pconstant

-- | @since 3.12.0
instance PTryFrom PData (PAsData (PFixed unit)) where
  type PTryFromExcess PData (PAsData (PFixed unit)) = PTryFromExcess PData (PAsData PInteger)
  ptryFrom' d k = ptryFrom' @_ @(PAsData PInteger) d $ k . first punsafeCoerce

-- TODO: This should be moved to either to plutarch-numeric or other module
class DivideSemigroup a where
  divide :: a -> a -> a

class DivideSemigroup a => DivideMonoid a where
  one :: a

-- | @since 3.12.0
instance KnownNat u => DivideSemigroup (Term s (PFixed u)) where
  divide (pto -> x) (pto -> y) =
    pcon . PFixed $ pdiv # (x * pconstant (natVal (Proxy @u))) # y

-- | @since 3.12.0
instance KnownNat u => DivideMonoid (Term s (PFixed u)) where
  one = 1

-- | @since 3.12.0
instance KnownNat u => A.AdditiveSemigroup (Term s (PFixed u)) where
  (+) = (+)

-- | @since 3.12.0
instance KnownNat u => A.AdditiveMonoid (Term s (PFixed u)) where
  zero = pcon . PFixed $ pconstant 0

{- | Convert given fixed into Ada value. Input should be Ada value with decimals; outputs
 will be lovelace values in integer.

 @since 3.9.0
-}
fixedToAdaValue ::
  forall (s :: S) (unit :: Nat).
  KnownNat unit =>
  Term s (PFixed unit :--> PValue 'Sorted 'NonZero)
fixedToAdaValue =
  phoistAcyclic $
    plam $ \(pto -> dec) ->
      let adaValue = (pdiv # dec # pconstant (natVal (Proxy @unit))) * pconstant 1000000
       in Value.psingleton # pconstant "" # pconstant "" #$ adaValue

{- | Convert @PInteger@ to @PFixed@.

 @since 3.12.0
-}
fromPInteger ::
  forall (unit :: Nat) (s :: S).
  KnownNat unit =>
  Term s (PInteger :--> PFixed unit)
fromPInteger =
  phoistAcyclic $ plam $ \z -> pcon . PFixed $ pconstant (natVal (Proxy @unit)) * z

{- | Convert @PFixed@ to @Integer@. Values that are smaller than 1 will be lost.

 @since 3.12.0
-}
toPInteger ::
  forall (unit :: Nat) (s :: S).
  KnownNat unit =>
  Term s (PFixed unit :--> PInteger)
toPInteger =
  phoistAcyclic $ plam $ \d -> pdiv # pto d # pconstant (natVal (Proxy @unit))
