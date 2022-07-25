{-# LANGUAGE OverloadedRecordDot #-}

module Plutarch.Context.SubBuilder (
    buildTxOut,
    buildTxOuts,
    buildDatumHashPairs,
) where

import Plutarch.Context.Base

import Data.Foldable (Foldable (toList))
import Data.Maybe
import PlutusLedgerApi.V1.Contexts
import PlutusLedgerApi.V1.Scripts (
    Datum (..),
    DatumHash,
 )

type SubBuilder = BaseBuilder

buildTxOut :: UTXO -> TxOut
buildTxOut u = utxoToTxOut u

buildTxOuts :: SubBuilder -> [TxOut]
buildTxOuts x = utxoToTxOut <$> (toList $ x.bbOutputs)

buildDatumHashPairs :: SubBuilder -> [(DatumHash, Datum)]
buildDatumHashPairs x =
    catMaybes (utxoDatumPair <$> (toList $ x.bbInputs <> x.bbOutputs))
        <> (datumWithHash <$> (toList $ x.bbDatums))
