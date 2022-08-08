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
        bbSignatures
    ),
    Builder (..),
    unpack,
    yieldBaseTxInfo,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldMint,
    yieldOutDatums,
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
        txInfoSignatories
    ),
    fromList,
 )

{- | Builder that builds TxInfo.

 @since 2.0.0
-}
newtype TxInfoBuilder
    = TxInfoBuilder BaseBuilder
    deriving newtype (Semigroup, Monoid)

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
        (outs, outDat) = yieldOutDatums bbOutputs
        mintedValue = yieldMint bbMints
        extraDat = yieldExtraDatums bbDatums
        base = yieldBaseTxInfo builder

        txinfo =
            base
                { txInfoInputs = ins
                , txInfoOutputs = outs
                , txInfoData = fromList $ inDat <> outDat <> extraDat
                , txInfoMint = mintedValue
                , txInfoSignatories = toList $ bbSignatures
                }
     in txinfo

spends :: TxInfo -> [ScriptContext]
spends txinfo =
    [ ScriptContext txinfo (Spending . txInInfoOutRef $ ins)
    | ins <- txInfoInputs txinfo
    ]

mints :: TxInfo -> [ScriptContext]
mints _txinfo = undefined
