{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}

{- | Provideds a scott-encoded asset class type and utility functions
 NOTE: This module exports types of the same name as
 @Liqwid.Units.DataAssetClass@. It should be imported qualified.
-}
module Plutarch.Extra.Units.ScottAssetClass (
    -- * Exchange
    (:>),

    -- * AssetClass - Hask
    AssetClass (AssetClass, symbol, name),

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
) where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified GHC.Generics as GHC
import GHC.TypeLits (Symbol)

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
-- Exchange

{- | Represents an exchange from a to b.
     Let's say 1.00 ADA is worth 2.00 USD, then @ADA ':>' USD@ ought to be
     represented as 2.00.
-}
data (:>) (a :: Symbol) (b :: Symbol)

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
