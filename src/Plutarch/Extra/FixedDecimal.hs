{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.FixedDecimal (
    PFixedDecimal (..),
    DivideSemigroup (..),
    DivideMonoid (..),
    decimalToAdaValue,
    fromPInteger,
    toPInteger,
) where

import Control.Composition (on, (.*))
import Data.Bifunctor (first)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)
import qualified Generics.SOP as SOP
import Plutarch.Api.V1 (PValue)
import Plutarch.Api.V2 (AmountGuarantees, KeyGuarantees)
import Plutarch.Bool (PEq, POrd, PPartialOrd)
import Plutarch.Extra.Function (pflip)
import Plutarch.Extra.Value (psingletonValue)
import Plutarch.Integer (PInteger, PIntegral (pdiv))
import Plutarch.Num (PNum (..))
import qualified Plutarch.Numeric.Additive as A (
    AdditiveMonoid (..),
    AdditiveSemigroup (..),
 )
import Plutarch.Prelude (
    DerivePlutusType (..),
    PAsData,
    PData,
    PIsData,
    PTryFrom,
    PlutusType,
    PlutusTypeNewtype,
    S,
    Term,
    pcon,
    pconstant,
    phoistAcyclic,
    plam,
    pto,
    (#),
    (#$),
    type (:-->),
 )
import Plutarch.Show (PShow)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)

{- | Fixed width decimal. Decimal point will be given through typelit.
 This would be used for representing Ada value with some Lovelace changes.

 @since 1.0.0
-}
newtype PFixedDecimal (unit :: Nat) (s :: S)
    = PFixedDecimal (Term s PInteger)
    deriving stock
        ( -- | @since 1.0.0
          Generic
        )
    deriving anyclass
        ( -- | @since 1.0.0
          SOP.Generic
        , -- | @since 1.0.0
          PlutusType
        , -- | @since 1.0.0
          PIsData
        , -- | @since 1.4.0
          PEq
        , -- | @since 1.4.0
          PPartialOrd
        , -- | @since 1.0.0
          POrd
        , -- | @since 1.0.0
          PShow
        )

-- | @since 1.4.0
instance DerivePlutusType (PFixedDecimal a) where
    type DPTStrat _ = PlutusTypeNewtype

instance KnownNat u => PNum (PFixedDecimal u) where
    (#*) =
        (pcon . PFixedDecimal)
            .* (pflip # pdiv # pconstant (natVal (Proxy @u)) #)
            .* (#*) `on` punsafeCoerce
    pfromInteger = pcon . PFixedDecimal . (* pconstant (natVal (Proxy @u))) . pconstant

-- | @since 1.0.0
instance PTryFrom PData (PAsData (PFixedDecimal unit)) where
    type PTryFromExcess PData (PAsData (PFixedDecimal unit)) = PTryFromExcess PData (PAsData PInteger)
    ptryFrom' d k = ptryFrom' @_ @(PAsData PInteger) d $ k . first punsafeCoerce

-- TODO: This should be moved to either to plutarch-numeric or other module
class DivideSemigroup a where
    divide :: a -> a -> a

class DivideSemigroup a => DivideMonoid a where
    one :: a

-- | @since 1.0.0
instance KnownNat u => DivideSemigroup (Term s (PFixedDecimal u)) where
    divide (pto -> x) (pto -> y) =
        pcon . PFixedDecimal $ pdiv # (x * pconstant (natVal (Proxy @u))) # y

-- | @since 1.0.0
instance KnownNat u => DivideMonoid (Term s (PFixedDecimal u)) where
    one = 1

-- | @since 1.0.0
instance KnownNat u => A.AdditiveSemigroup (Term s (PFixedDecimal u)) where
    (+) = (+)

-- | @since 1.0.0
instance KnownNat u => A.AdditiveMonoid (Term s (PFixedDecimal u)) where
    zero = pcon . PFixedDecimal $ pconstant 0

{- | Convert given decimal into Ada value. Input should be Ada value with decimals; outputs
 will be lovelace values in integer.

 @since 1.0.0
-}
decimalToAdaValue ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (unit :: Nat).
    KnownNat unit =>
    Term s (PFixedDecimal unit :--> PValue keys amounts)
decimalToAdaValue =
    phoistAcyclic $
        plam $ \(pto -> dec) ->
            let adaValue = (pdiv # dec # pconstant (natVal (Proxy @unit))) * pconstant 1000000
             in psingletonValue # pconstant "" # pconstant "" #$ adaValue

{- | Convert @PInteger@ to @PFixedDecimal@.

 @since 1.0.0
-}
fromPInteger ::
    forall (unit :: Nat) (s :: S).
    KnownNat unit =>
    Term s (PInteger :--> PFixedDecimal unit)
fromPInteger =
    phoistAcyclic $ plam $ \z -> pcon . PFixedDecimal $ pconstant (natVal (Proxy @unit)) * z

{- | Convert @PFixedDecimal@ to @Integer@. Values that are smaller than 1 will be lost.

 @since 1.0.0
-}
toPInteger ::
    forall (unit :: Nat) (s :: S).
    KnownNat unit =>
    Term s (PFixedDecimal unit :--> PInteger)
toPInteger =
    phoistAcyclic $ plam $ \d -> pdiv # pto d # pconstant (natVal (Proxy @unit))
