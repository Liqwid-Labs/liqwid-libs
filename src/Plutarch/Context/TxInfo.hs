{-# LANGUAGE RecordWildCards #-}
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
import Optics (lens)
import Plutarch.Context.Base (
  BaseBuilder (
    BB,
    bbDatums,
    bbInputs,
    bbMints,
    bbOutputs,
    bbRedeemers,
    bbReferenceInputs,
    bbSignatures
  ),
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
    txInfoSignatories
  ),
  fromList,
 )

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
buildTxInfo (unpack -> builder@BB {..}) =
  let (ins, inDat) = yieldInInfoDatums bbInputs
      (refin, _) = yieldInInfoDatums bbReferenceInputs
      (outs, outDat) = yieldOutDatums bbOutputs
      mintedValue = yieldMint bbMints
      extraDat = yieldExtraDatums bbDatums
      base = yieldBaseTxInfo builder
      redeemerMap = yieldRedeemerMap bbInputs bbMints

      txinfo =
        base
          { txInfoInputs = ins
          , txInfoReferenceInputs = refin
          , txInfoOutputs = outs
          , txInfoData = fromList $ inDat <> outDat <> extraDat
          , txInfoMint = mintedValue
          , txInfoSignatories = toList bbSignatures
          , txInfoRedeemers = fromList $ toList bbRedeemers <> redeemerMap
          }
   in txinfo

spends :: TxInfo -> [ScriptContext]
spends txinfo =
  [ ScriptContext txinfo (Spending . txInInfoOutRef $ ins)
  | ins <- txInfoInputs txinfo
  ]

mints :: TxInfo -> [ScriptContext]
mints _txinfo = undefined
