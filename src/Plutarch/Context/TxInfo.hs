module Plutarch.Context.TxInfo (
    buildTxInfo,
    spends,
    mints,
) where

import Control.Monad.Cont
import Data.Foldable (Foldable (toList))
import Plutarch.Context.Base
import PlutusLedgerApi.V1

buildTxInfo :: BaseBuilder -> Either String TxInfo
buildTxInfo builder = flip runContT Right $ do
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

spends :: TxInfo -> [ScriptContext]
spends txinfo =
    [ ScriptContext txinfo (Spending . txInInfoOutRef $ ins)
    | ins <- txInfoInputs txinfo
    ]

mints :: TxInfo -> [ScriptContext]
mints _txinfo = undefined
