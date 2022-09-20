{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Provides a Data-encoded asset class type and conversion functions
 NOTE: This module exports types of the same name as @Liqwid.Units.Currency@.
-}
module Plutarch.Extra.Units.DataAssetClass (
    -- * Data-encoded AssetClass
    AssetClass (AssetClass, symbol, name),
    PAssetClass (PAssetClass),

    -- * Utils to convert to/from Scott-encoded AssetClass
    toScottEncoding,
    fromScottEncoding,
    ptoScottEncoding,
    pfromScottEncoding,
    pviaScottEncoding,
) where

--------------------------------------------------------------------------------

import qualified GHC.Generics as GHC
import GHC.TypeLits (Symbol)

--------------------------------------------------------------------------------

import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (
    PCurrencySymbol,
    PTokenName,
 )
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.IsData (
    DerivePConstantViaDataList (..),
    ProductIsData (..),
 )
import Plutarch.Lift (
    PConstantDecl (..),
    PUnsafeLiftDecl (..),
 )

import qualified PlutusLedgerApi.V1.Value as Plutus
import qualified PlutusTx

--------------------------------------------------------------------------------

import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import qualified Plutarch.Extra.Units.ScottAssetClass as Scott
import Plutarch.Orphans ()

--------------------------------------------------------------------------------

-- | Data-encoded AssetClass without the phantom-tag
data AssetClass = AssetClass
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
    deriving (PlutusTx.ToData, PlutusTx.FromData) via ProductIsData AssetClass
    deriving
        (PConstantDecl)
        via ( DerivePConstantViaDataList
                AssetClass
                PAssetClass
            )

-- | Tagless data-encoded AssetClass
newtype PAssetClass (s :: S)
    = PAssetClass
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

instance DerivePlutusType PAssetClass where
    type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PAssetClass where
    type PLifted PAssetClass = AssetClass

--------------------------------------------------------------------------------

-- | Convert from `AssetClass` to `Scott.AssetClass`
toScottEncoding :: forall (tag :: Symbol). AssetClass -> Scott.AssetClass tag
toScottEncoding (AssetClass sym tk) = Scott.AssetClass sym tk

-- | Convert from `Scott.AssetClass` to `AssetClass`
fromScottEncoding :: forall (tag :: Symbol). Scott.AssetClass tag -> AssetClass
fromScottEncoding (Scott.AssetClass sym tk) = AssetClass sym tk

-- | Convert from `PAssetClass` to `PScott.AssetClass`
ptoScottEncoding ::
    forall (tag :: Symbol) (s :: S).
    Term
        s
        ( PAsData PAssetClass
            :--> Scott.PAssetClass tag
        )
ptoScottEncoding = phoistAcyclic $
    plam $ \cls ->
        pletFields @["symbol", "name"] (pto cls) $
            \cls' -> pcon $ Scott.PAssetClass cls'.symbol cls'.name

-- | Convert from `Scott.PAssetClass` to `PAssetClass`
pfromScottEncoding ::
    forall (tag :: Symbol) (s :: S).
    Term
        s
        ( Scott.PAssetClass tag
            :--> PAsData PAssetClass
        )
pfromScottEncoding = phoistAcyclic $
    plam $ \cls -> pmatch cls $
        \(Scott.PAssetClass sym tk) ->
            pdata $
                mkRecordConstr
                    PAssetClass
                    ( #symbol .= sym
                        .& #name .= tk
                    )

{- | Wrap a function using the Scott-encoded AssetClass to one using the
 Data-Encoded version
-}
pviaScottEncoding ::
    forall (tag :: Symbol) (a :: PType).
    ClosedTerm (Scott.PAssetClass tag :--> a) ->
    ClosedTerm (PAsData PAssetClass :--> a)
pviaScottEncoding fn = phoistAcyclic $
    plam $ \cls ->
        fn #$ ptoScottEncoding # cls
