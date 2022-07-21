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
    spends,
    mints,
    buildTxInfo,
    buildTxInfoUnsafe,
) where

import Control.Monad.Cont (ContT (runContT))
import Data.Foldable (Foldable (toList))
import Plutarch.Context.Base (
    BaseBuilder (bbDatums, bbInputs, bbMints, bbOutputs, bbSignatures),
    Builder (unpack),
    yieldBaseTxInfo,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldMint,
    yieldOutDatums,
 )
import PlutusLedgerApi.V1 (
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
 )

type TxInfoBuilder = BaseBuilder

buildTxInfo :: TxInfoBuilder -> Either String TxInfo
buildTxInfo (unpack -> builder) = flip runContT Right $ do
    let bb = unpack builder

    (ins, inDat) <- yieldInInfoDatums (bbInputs bb) builder
    (outs, outDat) <- yieldOutDatums (bbOutputs bb)
    mintedValue <- yieldMint (bbMints bb)
    extraDat <- yieldExtraDatums (bbDatums bb)
    base <- yieldBaseTxInfo builder

    let txinfo =
            base
                { txInfoInputs = ins
                , txInfoOutputs = outs
                , txInfoData = inDat <> outDat <> extraDat
                , txInfoMint = mintedValue
                , txInfoSignatories = toList $ bbSignatures bb
                }

    return txinfo

buildTxInfoUnsafe :: TxInfoBuilder -> TxInfo
buildTxInfoUnsafe = either error id . buildTxInfo

spends :: TxInfo -> [ScriptContext]
spends txinfo =
    [ ScriptContext txinfo (Spending . txInInfoOutRef $ ins)
    | ins <- txInfoInputs txinfo
    ]

mints :: TxInfo -> [ScriptContext]
mints _txinfo = undefined
