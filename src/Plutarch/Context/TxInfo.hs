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
        bbReferenceInputs,
        bbSignatures
    ),
    Builder (pack, _bb),
    unpack,
    yieldBaseTxInfo,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldMint,
    yieldMintRedeemerMap,
    yieldOutDatums,
    yieldScriptInputRedeemerMap,
 )
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

{- | Builds `TxInfo` from TxInfoBuilder.

 @since 2.0.0
-}
buildTxInfo :: TxInfoBuilder -> TxInfo
buildTxInfo (unpack -> builder@BB{..}) =
    let (ins, inDat) = yieldInInfoDatums bbInputs
        (refin, _) = yieldInInfoDatums bbReferenceInputs
        (outs, outDat) = yieldOutDatums bbOutputs
        mintedValue = yieldMint bbMints
        extraDat = yieldExtraDatums bbDatums
        base = yieldBaseTxInfo builder
        redeemerMap =
            AssocMap.fromList $
                mconcat
                    [ yieldMintRedeemerMap bbMints
                    , yieldScriptInputRedeemerMap bbInputs
                    ]

        txinfo =
            base
                { txInfoInputs = ins
                , txInfoReferenceInputs = refin
                , txInfoOutputs = outs
                , txInfoData = fromList $ inDat <> outDat <> extraDat
                , txInfoMint = mintedValue
                , txInfoSignatories = toList bbSignatures
                , txInfoRedeemers = redeemerMap
                }
     in txinfo

spends :: TxInfo -> [ScriptContext]
spends txinfo =
    [ ScriptContext txinfo (Spending . txInInfoOutRef $ ins)
    | ins <- txInfoInputs txinfo
    ]

mints :: TxInfo -> [ScriptContext]
mints _txinfo = undefined
