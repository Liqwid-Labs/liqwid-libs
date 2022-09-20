{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Provideds a scott-encoded asset class type and utility functions
 NOTE: This module exports types of the same name as
 @Liqwid.Units.DataAssetClass@. It should be imported qualified.
-}
module Plutarch.Extra.AssetClass (
    -- * AssetClass - Hask
    AssetClass (AssetClass, symbol, name),
    assetClassValue,

    -- * AssetClass - Plutarch
    PAssetClass (PAssetClass, psymbol, pname),
    adaSymbolData,
    isAdaClass,
    adaClass,
    padaClass,
    emptyTokenNameData,
    pcoerceCls,
    psymbolAssetClass,
    pconstantCls,

    -- * AssetClassData - Hask
    AssetClassData (AssetClassData, symbol, name),

    -- * AssetClassData - Plutarch
    PAssetClassData (PAssetClassData),

    -- * Scott <-> Data conversions
    toScottEncoding,
    ptoScottEncoding,
    fromScottEncoding,
    pfromScottEncoding,
    pviaScottEncoding,
) where

--------------------------------------------------------------------------------

import GHC.TypeLits (Symbol)

--------------------------------------------------------------------------------

import qualified Data.Aeson as Aeson
import Data.Tagged (Tagged, untag)
import qualified Generics.SOP as SOP
import qualified PlutusLedgerApi.V1.Value as Value

--------------------------------------------------------------------------------

import Plutarch.DataRepr (PDataFields)

import Plutarch.Extra.IsData (
    DerivePConstantViaDataList (DerivePConstantViaDataList),
    ProductIsData (ProductIsData),
 )
import Plutarch.Lift (
    PConstantDecl,
    PUnsafeLiftDecl (PLifted),
 )

import qualified PlutusTx

--------------------------------------------------------------------------------

import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified GHC.Generics as GHC

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (
    PCurrencySymbol,
    PTokenName,
 )
import Plutarch.Unsafe (punsafeCoerce)

--------------------------------------------------------------------------------

import Plutarch.Orphans ()

--------------------------------------------------------------------------------

import qualified PlutusLedgerApi.V1.Value as Plutus

--------------------------------------------------------------------------------
-- AssetClass & Variants

-- | Version of Plutus AssetClass, with CurrencyUnit tag
data AssetClass (tag :: Symbol) = AssetClass
    { symbol :: Plutus.CurrencySymbol
    , name :: Plutus.TokenName
    }
    deriving stock (Eq, Ord, Show, GHC.Generic)
    deriving anyclass
        ( ToJSON
        , FromJSON
        , FromJSONKey
        , ToJSONKey
        )

-- | Scott-encoded AssetClass
data PAssetClass (unit :: Symbol) (s :: S) = PAssetClass
    { psymbol :: Term s (PAsData PCurrencySymbol)
    , pname :: Term s (PAsData PTokenName)
    }
    deriving stock (GHC.Generic)
    deriving anyclass (PEq, PlutusType) -- Scott-encoded

instance DerivePlutusType (PAssetClass unit) where
    type DPTStrat _ = PlutusTypeScott

pconstantCls ::
    forall (unit :: Symbol) (s :: S).
    AssetClass unit ->
    Term s (PAssetClass unit)
pconstantCls (AssetClass sym tk) =
    pcon $
        PAssetClass (pconstantData sym) (pconstantData tk)

-- | Coerce the unit tag of a PAssetClass''
pcoerceCls ::
    forall (b :: Symbol) (a :: Symbol) (s :: S).
    Term s (PAssetClass a) ->
    Term s (PAssetClass b)
pcoerceCls = punsafeCoerce

-- | Construct a PAssetClass'' with empty TokenName
psymbolAssetClass ::
    forall (unit :: Symbol) (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    PAssetClass unit s
psymbolAssetClass sym = PAssetClass sym emptyTokenNameData

--------------------------------------------------------------------------------
-- Simple Helpers

-- | Check whether an AssetClass is the Ada Class
isAdaClass :: AssetClass "Underlying" -> Bool
isAdaClass (AssetClass s n) = s == s' && n == n'
  where
    (AssetClass s' n') = adaClass

-- | Ada CurrencySymbol - ""
adaSymbolData :: forall (s :: S). Term s (PAsData PCurrencySymbol)
adaSymbolData = pconstantData ""

-- | Ada AssetClass - ("", "")
adaClass :: AssetClass "Ada"
adaClass = AssetClass "" ""

-- | Ada AssetClass - Plutarch-level
padaClass :: forall (s :: S). Term s (PAssetClass "Ada")
padaClass = pconstantCls adaClass

-- | Empty String as a 'PTokenName'
emptyTokenNameData :: forall (s :: S). Term s (PAsData PTokenName)
emptyTokenNameData = pconstantData ""

----------------------------------------
-- Data-Encoded version

-- | Data-encoded AssetClass without the phantom-tag
data AssetClassData = AssetClassData
    { symbol :: Plutus.CurrencySymbol
    , name :: Plutus.TokenName
    }
    deriving stock (Eq, Ord, Show, GHC.Generic)
    deriving anyclass
        ( Aeson.ToJSON
        , Aeson.FromJSON
        , Aeson.FromJSONKey
        , Aeson.ToJSONKey
        , SOP.Generic
        )
    deriving
        (PlutusTx.ToData, PlutusTx.FromData)
        via Plutarch.Extra.IsData.ProductIsData AssetClassData
    deriving
        (Plutarch.Lift.PConstantDecl)
        via ( Plutarch.Extra.IsData.DerivePConstantViaDataList
                AssetClassData
                PAssetClassData
            )

-- | Tagless data-encoded AssetClass
newtype PAssetClassData (s :: S)
    = PAssetClassData
        ( Term
            s
            ( PDataRecord
                '[ "symbol" ':= PCurrencySymbol
                 , "name" ':= PTokenName
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving anyclass (PlutusType, PEq, PIsData, PDataFields, PShow)

instance DerivePlutusType PAssetClassData where
    type DPTStrat _ = PlutusTypeNewtype

instance Plutarch.Lift.PUnsafeLiftDecl PAssetClassData where
    type PLifted PAssetClassData = AssetClassData

-- | Convert from `AssetClassData` to `AssetClass`
toScottEncoding :: forall (tag :: Symbol). AssetClassData -> AssetClass tag
toScottEncoding (AssetClassData sym tk) = AssetClass sym tk

-- | Convert from `AssetClass` to `AssetClassData`
fromScottEncoding :: forall (tag :: Symbol). AssetClass tag -> AssetClassData
fromScottEncoding (AssetClass sym tk) = AssetClassData sym tk

-- | Convert from `PAssetClassData` to `PAssetClass`
ptoScottEncoding ::
    forall (tag :: Symbol) (s :: S).
    Term
        s
        ( PAsData PAssetClassData
            :--> PAssetClass tag
        )
ptoScottEncoding = phoistAcyclic $
    plam $ \cls ->
        pletFields @["symbol", "name"] (pto cls) $
            \cls' -> pcon $ PAssetClass cls'.symbol cls'.name

-- | Convert from `PAssetClass` to `PAssetClassData`
pfromScottEncoding ::
    forall (tag :: Symbol) (s :: S).
    Term
        s
        ( PAssetClass tag
            :--> PAsData PAssetClassData
        )
pfromScottEncoding = phoistAcyclic $
    plam $ \cls -> pmatch cls $
        \(PAssetClass sym tk) ->
            pdata $
                mkRecordConstr
                    PAssetClassData
                    ( #symbol .= sym
                        .& #name .= tk
                    )

{- | Wrap a function using the Scott-encoded AssetClass to one using the
 Data-Encoded version
-}
pviaScottEncoding ::
    forall (tag :: Symbol) (a :: PType).
    ClosedTerm (PAssetClass tag :--> a) ->
    ClosedTerm (PAsData PAssetClassData :--> a)
pviaScottEncoding fn = phoistAcyclic $
    plam $ \cls ->
        fn #$ ptoScottEncoding # cls

-- | Version of assetClassValue for tagged AssetClass & Tagged
assetClassValue ::
    forall (unit :: Symbol).
    AssetClass unit ->
    Tagged unit Integer ->
    Value.Value
assetClassValue (AssetClass sym tk) q = Value.singleton sym tk $ untag q
