{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.Rewarding
 Copyright: (C) Liqwid Labs 2022
 Maintainer: Seungheon Oh <seungheon@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Builder for rewarding contexts. 'RewardingBuilder' is an instance of 'Semigroup',
 which allows combining the results of this API's functions into a larger
 'RewardingBuilder' using '<>'.
-}
module Plutarch.Context.Rewarding (
  -- * Types
  RewardingBuilder,

  -- * Input
  withRewarding,

  -- * builder
  buildRewarding',
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
  Credential (PubKeyCredential),
  ScriptContext (ScriptContext),
  ScriptPurpose (Rewarding),
  StakingCredential (StakingHash),
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

{- | A context builder for Rewarding. Corresponds to
 'Plutus.V1.Ledger.Contexts.Rewarding' specifically.

 @since 2.8.0
-}
data RewardingBuilder = RB BaseBuilder (Maybe StakingCredential)
  deriving stock
    ( -- | @since 2.8.0
      Show
    )

-- | @since 2.8.0
instance
  (k ~ A_Lens, a ~ BaseBuilder, b ~ BaseBuilder) =>
  LabelOptic "inner" k RewardingBuilder RewardingBuilder a b
  where
  labelOptic = lens (\(RB x _) -> x) $ \(RB _ cs) inner' -> RB inner' cs

-- | @since 2.8.0
instance
  (k ~ A_Lens, a ~ Maybe StakingCredential, b ~ Maybe StakingCredential) =>
  LabelOptic "rewardingCred" k RewardingBuilder RewardingBuilder a b
  where
  labelOptic = lens (\(RB _ x) -> x) $ \(RB inner _) cs' -> RB inner cs'

-- | @since 2.8.0
instance Semigroup RewardingBuilder where
  RB inner _ <> RB inner' cs@(Just _) =
    RB (inner <> inner') cs
  RB inner cs <> RB inner' Nothing =
    RB (inner <> inner') cs

-- | @since 2.8.0
instance Monoid RewardingBuilder where
  mempty = RB mempty Nothing

-- | @since 2.8.0
instance Builder RewardingBuilder where
  _bb = #inner
  pack x = set #inner x (mempty :: RewardingBuilder)

instance Normalizer RewardingBuilder where
  mkNormalized' (RB bb cs) =
    RB (mkNormalized bb) cs

{- | Set CurrencySymbol for building Rewarding ScriptContext.

 @since 2.8.0
-}
withRewarding :: StakingCredential -> RewardingBuilder
withRewarding sc = RB mempty $ Just sc

{- | Builds @ScriptContext@ according to given configuration and
 @RewardingBuilder@.

 @since 2.8.0
-}
buildRewarding' ::
  RewardingBuilder ->
  ScriptContext
buildRewarding' builder@(unpack -> bb) =
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
      rewardCred = case view #rewardingCred builder of
        Just cred -> Rewarding cred
        Nothing -> Rewarding . StakingHash . PubKeyCredential $ ""
   in ScriptContext txinfo rewardCred
