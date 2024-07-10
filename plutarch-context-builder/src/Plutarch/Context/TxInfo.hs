{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.TxInfo
 Copyright: (C) Liqwid Labs 2022
 Maintainer: Seungheon Oh <seungheon.ooh@gmail.com>
 Portability: GHC only
 Stability: Experimental

 Builder for TxInfo and other utility functions that generates all
 possible Script Context from TxInfo.
-}
module Plutarch.Context.TxInfo (
  TxInfoBuilder (..),
  spends,
  mints,
  buildTxInfo,
) where

import Data.Foldable (Foldable (toList))
import Optics (lens, view)
import Plutarch.Context.Base (
  BaseBuilder,
  Builder (pack, _bb),
  unpack,
  yieldBaseTxInfo,
  yieldExtraDatums,
  yieldInInfoDatums,
  yieldMint,
  yieldOutDatums,
  yieldRedeemerMap,
 )
import Plutarch.Context.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V2 (
  ScriptContext (ScriptContext),
  ScriptPurpose (Spending),
  TxInInfo (txInInfoOutRef),
  TxInfo (
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoRedeemers,
    txInfoReferenceInputs,
    txInfoSignatories,
    txInfoWdrl
  ),
 )
import PlutusTx.AssocMap qualified as AssocMap

{- | Builder that builds TxInfo.

 @since 2.0.0
-}
newtype TxInfoBuilder
  = TxInfoBuilder BaseBuilder
  deriving (Semigroup, Monoid) via BaseBuilder

-- | @since 2.1.0
instance Builder TxInfoBuilder where
  _bb = lens (\(TxInfoBuilder x) -> x) (\_ b -> TxInfoBuilder b)
  pack = TxInfoBuilder

instance Normalizer TxInfoBuilder where
  mkNormalized' (TxInfoBuilder x) = TxInfoBuilder $ mkNormalized x

{- | Builds `TxInfo` from TxInfoBuilder.

 @since 2.0.0
-}
buildTxInfo :: TxInfoBuilder -> TxInfo
buildTxInfo (unpack -> builder) =
  let (ins, inDat) = yieldInInfoDatums . view #inputs $ builder
      (refin, _) = yieldInInfoDatums . view #referenceInputs $ builder
      (outs, outDat) = yieldOutDatums . view #outputs $ builder
      mintedValue = yieldMint . view #mints $ builder
      extraDat = yieldExtraDatums . view #datums $ builder
      base = yieldBaseTxInfo builder
      redeemerMap = yieldRedeemerMap (view #inputs builder) (view #mints builder)
      txinfo =
        base
          { txInfoInputs = ins
          , txInfoReferenceInputs = refin
          , txInfoOutputs = outs
          , txInfoData = AssocMap.unsafeFromList $ inDat <> outDat <> extraDat
          , txInfoMint = mintedValue
          , txInfoSignatories = toList (view #signatures builder)
          , txInfoRedeemers = AssocMap.unsafeFromList $ toList (view #redeemers builder) <> redeemerMap
          , txInfoWdrl = AssocMap.unsafeFromList $ toList (view #withdrawals builder)
          }
   in txinfo

spends :: TxInfo -> [ScriptContext]
spends txinfo =
  [ ScriptContext txinfo (Spending . txInInfoOutRef $ ins)
  | ins <- txInfoInputs txinfo
  ]

mints :: TxInfo -> [ScriptContext]
mints _txinfo = undefined
