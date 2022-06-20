{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.FixedDecimal (
    PFixedDecimal (..),
    DivideSemigroup (..),
    DivideMonoid (..),
    decimalToAdaValue,
) where

import Plutarch.Api.V1
import Plutarch.Api.V1.Value
import Plutarch.Bool
import Plutarch.Integer
import Plutarch.Prelude
import Plutarch.Unsafe

decimalUnit :: Term s PInteger
decimalUnit = 1000000

{- | Fixed width decimal. Decimal point is at 1,000,000.
 This would be used for representing Ada value with some Lovelace changes.

 @since 1.0.0
-}
newtype PFixedDecimal (s :: S)
    = PFixedDecimal (Term s PInteger)

-- | @since 1.0.0
deriving via
    (DerivePNewtype PFixedDecimal PInteger)
    instance
        (PlutusType PFixedDecimal)

-- | @since 1.0.0
deriving via
    (DerivePNewtype PFixedDecimal PInteger)
    instance
        PEq PFixedDecimal

-- | @since 1.0.0
deriving via
    (DerivePNewtype PFixedDecimal PInteger)
    instance
        POrd PFixedDecimal

-- | @since 1.0.0
instance Num (Term s PFixedDecimal) where
    (+) = (+)
    (-) = (-)
    (punsafeCoerce -> x) * (punsafeCoerce -> y) =
        punsafeCoerce $ pdiv # (x * y) # decimalUnit
    abs = abs
    signum = signum
    fromInteger = punsafeCoerce . (* decimalUnit) . pconstant

-- TODO: This should be moved to either to plutarch-numeric or other module
class DivideSemigroup a where
    divide :: a -> a -> a

class (DivideSemigroup a) => DivideMonoid a where
    one :: a

-- | @since 1.0.0
instance DivideSemigroup (Term s PFixedDecimal) where
    divide (punsafeCoerce -> x) (punsafeCoerce -> y) =
        punsafeCoerce $ pdiv # (x * decimalUnit) # y

-- | @since 1.0.0
instance DivideMonoid (Term s PFixedDecimal) where
    one = fromInteger 1

-- | @since 1.0.0
decimalToAdaValue ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s (PFixedDecimal :--> PValue keys amounts)
decimalToAdaValue =
    phoistAcyclic $
        plam $ \(punsafeCoerce -> dec) ->
            psingletonValue # pconstant "" # pconstant "" # dec
