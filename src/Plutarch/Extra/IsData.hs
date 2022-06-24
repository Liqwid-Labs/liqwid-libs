{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.IsData (
    -- * PlutusTx ToData/FromData derive-wrappers
    ProductIsData (..),
    EnumIsData (..),

    -- * Plutarch PIsData/PlutusType derive-wrappers
    PEnumData (..),
    DerivePConstantViaDataList (..),
    DerivePConstantViaEnum (..),

    -- * Functions for PEnumData types
    pmatchEnum,
    pmatchEnumFromData,
) where

--------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Data.Proxy (Proxy (..))
import Generics.SOP (
    All,
    IsProductType,
    hcmap,
    hcollapse,
    hctraverse,
    mapIK,
    mapKI,
    productTypeFrom,
    productTypeTo,
    unI,
 )
import qualified Generics.SOP as SOP
import Plutarch (PlutusType (pcon', pmatch'))
import Plutarch.Builtin (PIsData (pdataImpl, pfromDataImpl), pasInt)
import Plutarch.Extra.TermCont (pletC)
import Plutarch.Lift (PConstantDecl (..))
import Plutarch.Prelude (
    PData,
    PEq ((#==)),
    PInteger,
    PLift,
    PType,
    PlutusType (..),
    S,
    Term,
    Type,
    pif,
    pto,
    unTermCont,
    (#),
 )
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 (BuiltinData (BuiltinData))
import PlutusTx (Data (List), FromData (..), ToData (..), fromData, toData)
import Prelude

--------------------------------------------------------------------------------
-- ProductIsData

{- |
  Wrapper for deriving 'ToData', 'FromData' using the 'List' constructor of Data to represent a Product type.

  Uses 'gProductToBuiltinData', 'gproductFromBuiltinData'.

  @since 1.1.0
-}
newtype ProductIsData a = ProductIsData {unProductIsData :: a}

-- | Variant of 'PConstantViaData' using the List repr from 'ProductIsData'
newtype DerivePConstantViaDataList (h :: Type) (p :: PType) = DerivePConstantViaDataList h

{- |
  Generically convert a Product-Type to 'BuiltinData' with the 'List' repr.

  @since 1.1.0
-}
gProductToBuiltinData ::
    (IsProductType a repr, All ToData repr) => a -> BuiltinData
gProductToBuiltinData x =
    BuiltinData $ List $ hcollapse $ hcmap (Proxy @ToData) (mapIK toData) $ productTypeFrom x

{- |
  Generically convert a Product-type from a 'BuiltinData' 'List' repr.

  @since 1.1.0
-}
gProductFromBuiltinData ::
    forall (a :: Type) (repr :: [Type]).
    (IsProductType a repr, All FromData repr) =>
    BuiltinData ->
    Maybe a
gProductFromBuiltinData (BuiltinData (List xs)) = do
    prod <- SOP.fromList @repr xs
    productTypeTo <$> hctraverse (Proxy @FromData) (unI . mapKI fromData) prod
gProductFromBuiltinData _ = Nothing

-- | @since 1.1.0
instance
    (PlutusTx.FromData h, PlutusTx.ToData h, PLift p) =>
    PConstantDecl (DerivePConstantViaDataList h p)
    where
    type PConstantRepr (DerivePConstantViaDataList h p) = [PlutusTx.Data]
    type PConstanted (DerivePConstantViaDataList h p) = p
    pconstantToRepr (DerivePConstantViaDataList x) = case PlutusTx.toData x of
        (PlutusTx.List xs) -> xs
        _ -> error "ToData repr is not a List!"
    pconstantFromRepr = coerce (PlutusTx.fromData @h . PlutusTx.List)

-- | @since 1.1.0
instance (IsProductType a repr, All ToData repr) => ToData (ProductIsData a) where
    toBuiltinData = coerce (gProductToBuiltinData @a)

-- | @since 1.1.0
instance (IsProductType a repr, All FromData repr) => FromData (ProductIsData a) where
    fromBuiltinData = coerce (gProductFromBuiltinData @a)

--------------------------------------------------------------------------------
-- PEnumData

{- |
  Wrapper for deriving 'ToData', 'FromData' using an Integer representation via 'Enum'.

  @since 1.1.0
-}
newtype EnumIsData a = EnumIsData a

{- |
  Wrapper for deriving `PlutusType` & `PIsData` via a ToData repr derived with `EnumIsData`.

  @since 1.1.0
-}
newtype PEnumData (a :: PType) (s :: S) = PEnumData (a s)
    deriving
        ( -- | @since 1.1.0
          Enum
        , -- | @since 1.1.0
          Bounded
        )
        via (a s)

-- | @since 1.1.0
instance (Enum a) => ToData (EnumIsData a) where
    toBuiltinData = coerce $ toBuiltinData . toInteger . fromEnum @a

-- | @since 1.1.0
instance (Enum a) => FromData (EnumIsData a) where
    fromBuiltinData = coerce $ fmap (toEnum @a . fromInteger) . fromBuiltinData @Integer

-- | @since 1.1.0
instance
    ( forall s. Enum (a s)
    , forall s. Bounded (a s)
    , forall s. Eq (a s)
    ) =>
    PlutusType (PEnumData a)
    where
    type PInner (PEnumData a) _ = PInteger
    pmatch' = pmatchEnum
    pcon' = fromInteger . toInteger . fromEnum

-- | @since 1.1.0
instance forall (a :: PType). PIsData (PEnumData a) where
    pfromDataImpl d =
        punsafeCoerce (pfromDataImpl @PInteger $ punsafeCoerce d)

    pdataImpl x =
        pdataImpl $ pto x

{- |
  Wrapper for deriving `PConstantDecl` using an Integer representation via 'Enum'.

  @since 1.1.0
-}
newtype DerivePConstantViaEnum (h :: Type) (p :: PType)
    = DerivePConstantEnum h

-- | @since 1.1.0
instance (PLift p, Enum h) => PConstantDecl (DerivePConstantViaEnum h p) where
    type PConstantRepr (DerivePConstantViaEnum h p) = Integer
    type PConstanted (DerivePConstantViaEnum h p) = p

    pconstantToRepr = toInteger . fromEnum @h . coerce
    pconstantFromRepr = Just . coerce . toEnum @h . fromInteger

-- | Safely enumerate all the cases.
safeCases :: forall a. (Bounded a, Enum a) => [a]
safeCases = enumFrom minBound

{- |
  Pattern match over the integer-repr of a Bounded Enum type.

  @since 1.1.0
-}
pmatchEnum ::
    forall (a :: Type) (b :: PType) (s :: S).
    (Bounded a, Enum a) =>
    Term s PInteger ->
    (a -> Term s b) ->
    Term s b
pmatchEnum x f = unTermCont $ do
    x' <- pletC x

    let branch :: a -> Term s b -> Term s b
        branch n =
            pif
                (x' #== (fromInteger . toInteger . fromEnum $ n))
                (f n)

    pure $ foldr branch (f maxBound) safeCases

{- |
  Pattern match PData as a Bounded Enum. Useful for Redeemers.

  @since 1.1.0
-}
pmatchEnumFromData ::
    forall (a :: Type) (b :: PType) (s :: S).
    (Bounded a, Enum a) =>
    Term s PData ->
    (Maybe a -> Term s b) ->
    Term s b
pmatchEnumFromData d f = unTermCont $ do
    x <- pletC $ pasInt # d

    let branch :: a -> Term s b -> Term s b
        branch n =
            pif
                (x #== (fromInteger . toInteger . fromEnum $ n))
                (f $ Just n)

    pure $ foldr branch (f Nothing) safeCases
