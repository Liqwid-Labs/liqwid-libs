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
import Data.Maybe (mapMaybe)
import Optics (lens, view)
import Plutarch.Context.Base (
  BaseBuilder,
  Builder (..),
  UTXO,
  datumWithHash,
  unpack,
  utxoDatumPair,
  utxoToTxOut,
  yieldInInfoDatums,
 )
import Plutarch.Context.Internal (Normalizer (mkNormalized'), mkNormalized)
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
  deriving
    ( -- | @since 2.0.0
      Semigroup
    , -- | @since 2.0.0
      Monoid
    )
    via BaseBuilder

-- | @since 2.0.0
instance Builder SubBuilder where
  _bb = lens (\(SubBuilder x) -> x) (\_ b -> SubBuilder b)
  pack = SubBuilder

-- | @since 2.0.0
instance Normalizer SubBuilder where
  mkNormalized' (SubBuilder x) = SubBuilder $ mkNormalized x

{- | Builds TxOut from `UTXO`.

 @since 2.0.0
-}
buildTxOut :: UTXO -> TxOut
buildTxOut = utxoToTxOut

{- | Builds TxInInfo from `UTXO`.

 @since 2.0.0
-}
buildTxInInfo :: UTXO -> Maybe TxInInfo
buildTxInInfo u = do
  txid <- view #txId u
  txidx <- view #txIdx u
  return $ TxInInfo (TxOutRef txid txidx) (utxoToTxOut u)

{- | Builds all TxOuts from given builder.

 @since 2.0.0
-}
buildTxOuts :: SubBuilder -> [TxOut]
buildTxOuts (unpack -> bb) = utxoToTxOut <$> toList (view #outputs bb)

{- | Builds all TxInInfos from given builder. Returns reason when failed.

 @since 2.1.0
-}
buildTxInInfos :: SubBuilder -> [TxInInfo]
buildTxInInfos (unpack -> bb) =
  fst $ yieldInInfoDatums (view #inputs bb)

{- | Builds Datum-Hash pair from all inputs, outputs, extra data of given builder.

 @since 2.0.0
-}
buildDatumHashPairs :: SubBuilder -> [(DatumHash, Datum)]
buildDatumHashPairs (unpack -> bb) =
  mapMaybe utxoDatumPair (toList (view #inputs bb <> view #outputs bb))
    <> (datumWithHash <$> toList (view #datums bb))
