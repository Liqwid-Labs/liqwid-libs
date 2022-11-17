{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.Rewarding
 Copyright: (C) Liqwid Labs 2022
 Maintainer: Seungheon Oh <seungheon@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Builder for rewarding contexts. 'CertifyingBuilder' is an instance of 'Semigroup',
 which allows combining the results of this API's functions into a larger
 'CertifyingBuilder' using '<>'.
-}
module Plutarch.Context.Certifying (
  -- * Types
  CertifyingBuilder,

  -- * Input
  withCertifying,

  -- * builder
  buildCertifying',
) where

import Data.Foldable (Foldable (toList))
import Optics (A_Lens, LabelOptic (labelOptic), lens, set, view)
import Plutarch.Context.Base (
  BaseBuilder,
  Builder (pack, _bb),
  unpack,
  yieldBaseTxInfo,
  yieldExtraDatums,
  yieldInInfoDatums,
  yieldMint,
  yieldOutDatums,
  yieldRedeemerMap,
 )
import Plutarch.Context.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V2 (
  DCert (DCertGenesis),
  ScriptContext (ScriptContext),
  ScriptPurpose (Certifying),
  TxInfo (
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoRedeemers,
    txInfoReferenceInputs,
    txInfoSignatories,
    txInfoWdrl
  ),
  fromList,
 )

{- | A context builder for Certifying. Corresponds to
 'Plutus.V1.Ledger.Contexts.Certifying' specifically.

 @since 2.8.0
-}
data CertifyingBuilder = CB BaseBuilder (Maybe DCert)
  deriving stock
    ( -- | @since 2.8.0
      Show
    )

-- | @since 2.8.0
instance
  (k ~ A_Lens, a ~ BaseBuilder, b ~ BaseBuilder) =>
  LabelOptic "inner" k CertifyingBuilder CertifyingBuilder a b
  where
  labelOptic = lens (\(CB x _) -> x) $ \(CB _ cs) inner' -> CB inner' cs

-- | @since 2.8.0
instance
  (k ~ A_Lens, a ~ Maybe DCert, b ~ Maybe DCert) =>
  LabelOptic "certifyingDCert" k CertifyingBuilder CertifyingBuilder a b
  where
  labelOptic = lens (\(CB _ x) -> x) $ \(CB inner _) cs' -> CB inner cs'

-- | @since 2.8.0
instance Semigroup CertifyingBuilder where
  CB inner _ <> CB inner' cs@(Just _) =
    CB (inner <> inner') cs
  CB inner cs <> CB inner' Nothing =
    CB (inner <> inner') cs

-- | @since 2.8.0
instance Monoid CertifyingBuilder where
  mempty = CB mempty Nothing

-- | @since 2.8.0
instance Builder CertifyingBuilder where
  _bb = #inner
  pack x = set #inner x (mempty :: CertifyingBuilder)

instance Normalizer CertifyingBuilder where
  mkNormalized' (CB bb cs) =
    CB (mkNormalized bb) cs

{- | Set DCert for building Certifying ScriptContext.

 @since 2.8.0
-}
withCertifying :: DCert -> CertifyingBuilder
withCertifying sc = CB mempty $ Just sc

{- | Builds @ScriptContext@ according to given configuration and
 @CertifyingBuilder@.

 @since 2.8.0
-}
buildCertifying' ::
  CertifyingBuilder ->
  ScriptContext
buildCertifying' builder@(unpack -> bb) =
  let (ins, inDat) = yieldInInfoDatums . view #inputs $ bb
      (refin, _) = yieldInInfoDatums . view #referenceInputs $ bb
      (outs, outDat) = yieldOutDatums . view #outputs $ bb
      mintedValue = yieldMint . view #mints $ bb
      extraDat = yieldExtraDatums . view #datums $ bb
      base = yieldBaseTxInfo builder
      redeemerMap = yieldRedeemerMap (view #inputs bb) (view #mints bb)
      txinfo =
        base
          { txInfoInputs = ins
          , txInfoReferenceInputs = refin
          , txInfoOutputs = outs
          , txInfoData = fromList $ inDat <> outDat <> extraDat
          , txInfoMint = mintedValue
          , txInfoRedeemers = fromList $ toList (view #redeemers bb) <> redeemerMap
          , txInfoSignatories = toList . view #signatures $ bb
          , txInfoWdrl = fromList $ toList (view #withdrawals bb)
          }
      rewardCred = case view #certifyingDCert builder of
        Just dcert -> Certifying dcert
        Nothing -> Certifying DCertGenesis
   in ScriptContext txinfo rewardCred
