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
    buildTxInfoUnsafe,
) where

import Control.Monad.Cont ( ContT(runContT) )
import Data.Foldable ( Foldable(toList) )
import Plutarch.Context.Base
    ( BaseBuilder(bbDatums, bbInputs, bbMints, bbOutputs,
                  bbSignatures),
      Builder(..),
      yieldBaseTxInfo,
      yieldExtraDatums,
      yieldInInfoDatums,
      yieldMint,
      yieldOutDatums )
import PlutusLedgerApi.V2
    ( TxInfo(txInfoInputs, txInfoOutputs, txInfoData, txInfoMint,
             txInfoSignatories),
      TxInInfo(txInInfoOutRef),
      ScriptPurpose(Spending),
      fromList,
      ScriptContext(ScriptContext) )

{- | Builder that builds TxInfo.

 @since 2.0.0
-}
newtype TxInfoBuilder
    = TxInfoBuilder BaseBuilder
    deriving newtype (Semigroup, Monoid, Builder)

{- | Builds `TxInfo` from TxInfoBuilder.

 @since 2.0.0
-}
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
                , txInfoData = fromList $ inDat <> outDat <> extraDat
                , txInfoMint = mintedValue
                , txInfoSignatories = toList $ bbSignatures bb
                }

    return txinfo

-- | Builds TxInfo; it throwing error when builder fails.
buildTxInfoUnsafe :: TxInfoBuilder -> TxInfo
buildTxInfoUnsafe = either error id . buildTxInfo

spends :: TxInfo -> [ScriptContext]
spends txinfo =
    [ ScriptContext txinfo (Spending . txInInfoOutRef $ ins)
    | ins <- txInfoInputs txinfo
    ]

mints :: TxInfo -> [ScriptContext]
mints _txinfo = undefined
