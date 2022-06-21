{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.FixedDecimal (
    PFixedDecimal (..),
    DivideSemigroup (..),
    DivideMonoid (..),
    decimalToAdaValue,
) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, Nat, natVal)
import Plutarch.Api.V1 (AmountGuarantees, KeyGuarantees, PValue)
import Plutarch.Api.V1.Value (psingletonValue)
import Plutarch.Bool (PEq, POrd)
import Plutarch.Integer (PInteger, PIntegral (pdiv))
import Plutarch.Prelude (
    DerivePNewtype (..),
    PCon (pcon),
    PlutusType,
    S,
    Term,
    pconstant,
    phoistAcyclic,
    plam,
    pto,
    (#),
    (#$),
    type (:-->),
 )

{- | Fixed width decimal. Decimal point will be given through typelit.
 This would be used for representing Ada value with some Lovelace changes.

 @since 1.0.0
-}
newtype PFixedDecimal (unit :: Nat) (s :: S)
    = PFixedDecimal (Term s PInteger)

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PFixedDecimal u) PInteger)
    instance
        (PlutusType (PFixedDecimal u))

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PFixedDecimal u) PInteger)
    instance
        PEq (PFixedDecimal u)

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PFixedDecimal u) PInteger)
    instance
        POrd (PFixedDecimal u)

-- | @since 1.0.0
instance KnownNat u => Num (Term s (PFixedDecimal u)) where
    (+) = (+)
    (-) = (-)
    (pto -> x) * (pto -> y) =
        pcon . PFixedDecimal $ pdiv # (x * y) # pconstant (natVal (Proxy @u))
    abs = abs
    signum = signum
    fromInteger = pcon . PFixedDecimal . (* pconstant (natVal (Proxy @u))) . pconstant

-- TODO: This should be moved to either to plutarch-numeric or other module
class DivideSemigroup a where
    divide :: a -> a -> a

class DivideSemigroup a => DivideMonoid a where
    one :: a

-- | @since 1.0.0
instance KnownNat u => DivideSemigroup (Term s (PFixedDecimal u)) where
    divide (pto -> x) (pto -> y) =
        pcon . PFixedDecimal $ pdiv # (x * (pconstant $ natVal (Proxy @u))) # y

-- | @since 1.0.0
instance KnownNat u => DivideMonoid (Term s (PFixedDecimal u)) where
    one = fromInteger 1

-- | @since 1.0.0
decimalToAdaValue ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (unit :: Nat).
    KnownNat unit =>
    Term s (PFixedDecimal unit :--> PValue keys amounts)
decimalToAdaValue =
    phoistAcyclic $
        plam $ \(pto -> dec) ->
            let adaValue = (pdiv # dec # (pconstant (natVal (Proxy @unit)))) * pconstant 1000000
             in psingletonValue # pconstant "" # pconstant "" #$ adaValue
