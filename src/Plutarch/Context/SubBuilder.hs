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

import Plutarch.Context.Base

import Control.Monad.Cont (ContT (runContT))
import Data.Foldable (Foldable (toList))
import Data.Maybe (catMaybes)
import PlutusLedgerApi.V1.Contexts (TxInInfo (..), TxOut (..), TxOutRef (..))
import PlutusLedgerApi.V1.Scripts (
    Datum (..),
    DatumHash,
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
buildTxOut u = utxoToTxOut u

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
buildTxOuts (unpack -> BB{..}) = utxoToTxOut <$> (toList $ bbOutputs)

{- | Builds all TxInInfos from give builder. Returns reason when failed.

 @since 2.0.0
-}
buildTxInInfos :: SubBuilder -> Either String [TxInInfo]
buildTxInInfos b@(unpack -> BB{..}) = flip runContT Right $ do
    (ins, _) <- yieldInInfoDatums bbInputs b
    return ins

{- | Builds Datum-Hash pair from all inputs, outputs, extra data of given builder.

 @since 2.0.0
-}
buildDatumHashPairs :: SubBuilder -> [(DatumHash, Datum)]
buildDatumHashPairs (unpack -> BB{..}) =
    catMaybes (utxoDatumPair <$> (toList $ bbInputs <> bbOutputs))
        <> (datumWithHash <$> (toList $ bbDatums))
