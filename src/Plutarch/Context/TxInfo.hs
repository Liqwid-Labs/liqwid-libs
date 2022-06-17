{-# LANGUAGE ViewPatterns #-}
module Plutarch.Context.TxInfo (
    spends,
    mints,
    
    buildTxInfo,
    buildTxInfoUnsafe,
) where

import Control.Monad.Cont ( ContT(runContT) )
import Data.Foldable ( Foldable(toList) )
import Plutarch.Context.Base
    ( yieldBaseTxInfo,
      yieldExtraDatums,
      yieldInInfoDatums,
      yieldMint,
      yieldOutDatums,
      BaseBuilder(bbSignatures, bbInputs, bbOutputs, bbMints, bbDatums),
      Builder(unpack) )
import PlutusLedgerApi.V1
    ( TxInfo(txInfoInputs, txInfoOutputs, txInfoData, txInfoMint,
             txInfoSignatories),
      TxInInfo(txInInfoOutRef),
      ScriptContext(ScriptContext),
      ScriptPurpose(Spending) )
    
buildTxInfo :: Builder a => a -> Either String TxInfo
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

buildTxInfoUnsafe :: Builder a => a -> TxInfo
buildTxInfoUnsafe = either error id . buildTxInfo

spends :: TxInfo -> [ScriptContext]
spends txinfo =
    [ ScriptContext txinfo (Spending . txInInfoOutRef $ ins)
    | ins <- txInfoInputs txinfo
    ]

mints :: TxInfo -> [ScriptContext]
mints _txinfo = undefined
