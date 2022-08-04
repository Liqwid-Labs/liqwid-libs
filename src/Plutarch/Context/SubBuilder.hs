{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Context.SubBuilder (
    SubBuilder (..),
    buildTxOut,
    buildTxInInfo,
    buildTxOuts,
    buildTxInInfos,
    buildDatumHashPairs,
) where

import Data.Foldable (Foldable (toList))
import Data.Maybe (catMaybes)
import Plutarch.Context.Base (
    BaseBuilder (..),
    Builder (unpack),
    UTXO (..),
    datumWithHash,
    utxoDatumPair,
    utxoToTxOut,
    yieldInInfoDatums,
 )
import PlutusLedgerApi.V2 (
    Datum,
    DatumHash,
    TxInInfo (TxInInfo),
    TxOut,
    TxOutRef (TxOutRef),
 )

{- | Smaller builder that builds context smaller than TxInfo.

 @since 2.0.0
-}
newtype SubBuilder
    = SubBuilder BaseBuilder
    deriving newtype (Semigroup, Monoid, Builder)

{- | Builds TxOut from `UTXO`.

 @since 2.0.0
-}
buildTxOut :: UTXO -> TxOut
buildTxOut = utxoToTxOut

{- | Builds TxInInfo from `UTXO`.

 @since 2.0.0
-}
buildTxInInfo :: UTXO -> Maybe TxInInfo
buildTxInInfo u@(UTXO{..}) = do
    txid <- utxoTxId
    txidx <- utxoTxIdx
    return $ TxInInfo (TxOutRef txid txidx) (utxoToTxOut u)

{- | Builds all TxOuts from given builder.

 @since 2.0.0
-}
buildTxOuts :: SubBuilder -> [TxOut]
buildTxOuts (unpack -> BB{..}) = utxoToTxOut <$> toList bbOutputs

{- | Builds all TxInInfos from given builder. Returns reason when failed.

 @since 2.0.0
-}
buildTxInInfos :: SubBuilder -> [TxInInfo]
buildTxInInfos b@(unpack -> BB{..}) =
    fst $ yieldInInfoDatums bbInputs b

{- | Builds Datum-Hash pair from all inputs, outputs, extra data of given builder.

 @since 2.0.0
-}
buildDatumHashPairs :: SubBuilder -> [(DatumHash, Datum)]
buildDatumHashPairs (unpack -> BB{..}) =
    catMaybes (utxoDatumPair <$> toList (bbInputs <> bbOutputs))
        <> (datumWithHash <$> toList bbDatums)
