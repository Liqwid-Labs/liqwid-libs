{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.Spending
 Copyright: (C) Liqwid Labs 2022
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Builder for spending contexts. 'SpendingBuilder' is an instance of 'Semigroup',
 which allows combining the results of this API's functions into a larger
 'SpendingBuilder' using '<>'.
-}
module Plutarch.Context.Spending (
    -- * Types
    SpendingBuilder (..),

    -- * Inputs
    withSpendingUTXO,
    withSpendingOutRef,
    withSpendingOutRefId,
    withSpendingOutRefIdx,

    -- * builder
    buildSpending,
    checkBuildSpending,
    buildSpendingUnsafe,
) where

import Data.Foldable (Foldable (toList))
import Data.Maybe (fromJust)
import Plutarch.Context.Base (
    BaseBuilder (BB, bbDatums, bbInputs, bbMints, bbOutputs, bbSignatures),
    Builder (..),
    UTXO,
    utxoToTxOut,
    yieldBaseTxInfo,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldMint,
    yieldOutDatums,
 )
import PlutusLedgerApi.V2 (
    ScriptContext (ScriptContext),
    ScriptPurpose (Spending),
    TxId,
    TxInInfo (txInInfoOutRef, txInInfoResolved),
    TxInfo (
        txInfoData,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories
    ),
    TxOutRef (txOutRefId, txOutRefIdx),
    fromList,
 )

data ValidatorInputIdentifier
    = ValidatorUTXO UTXO
    | ValidatorOutRef TxOutRef
    | ValidatorOutRefId TxId
    | ValidatorOutRefIdx Integer

{- | A context builder for spending. Corresponds broadly to validators, and to
 'PlutusLedgerApi.V1.Contexts.Spending' specifically.

 @since 1.0.0
-}
data SpendingBuilder = SB
    { sbInner :: BaseBuilder
    , sbValidatorInput :: Maybe ValidatorInputIdentifier
    }

-- | @since 1.1.0
instance Builder SpendingBuilder where
    pack = flip SB Nothing
    unpack = sbInner

-- | @since 1.0.0
instance Semigroup SpendingBuilder where
    SB inner _ <> SB inner' (Just vin') =
        SB (inner <> inner') $ Just vin'
    SB inner vInRef <> SB inner' Nothing =
        SB (inner <> inner') vInRef

-- | @since 1.1.0
instance Monoid SpendingBuilder where
    mempty = SB mempty Nothing

{- | Set Validator Input with given UTXO. Note, the given UTXO should
   exist in the inputs, otherwise the builder would fail.

 @since 2.0.0
-}
withSpendingUTXO ::
    UTXO ->
    SpendingBuilder
withSpendingUTXO u =
    mempty
        { sbValidatorInput =
            Just $ ValidatorUTXO u
        }

{- | Set Validator Input with given TxOutRef. Note, input with given
   TxOutRef should exist, otherwise the builder would fail.

 @since 2.0.0
-}
withSpendingOutRef ::
    TxOutRef ->
    SpendingBuilder
withSpendingOutRef outref =
    mempty
        { sbValidatorInput =
            Just . ValidatorOutRef $ outref
        }

{- | Set Validator Input with given TxOutRefId. Note, input with given
   TxOutRefId should exist, otherwise the builder would fail.

 @since 2.0.0
-}
withSpendingOutRefId ::
    TxId ->
    SpendingBuilder
withSpendingOutRefId tid =
    mempty
        { sbValidatorInput =
            Just . ValidatorOutRefId $ tid
        }

{- | Set Validator Input with given TxOutRefIdx. Note, input with given
   TxOutRefIdx should exist, otherwise the builder would fail.

 @since 2.0.0
-}
withSpendingOutRefIdx ::
    Integer ->
    SpendingBuilder
withSpendingOutRefIdx tidx =
    mempty
        { sbValidatorInput =
            Just . ValidatorOutRefIdx $ tidx
        }

yieldValidatorInput ::
    [TxInInfo] ->
    ValidatorInputIdentifier ->
    Maybe TxOutRef
yieldValidatorInput ins = \case
    ValidatorUTXO utxo -> go txInInfoResolved (utxoToTxOut utxo)
    ValidatorOutRef outref -> go txInInfoOutRef outref
    ValidatorOutRefId tid -> go (txOutRefId . txInInfoOutRef) tid
    ValidatorOutRefIdx tidx -> go (txOutRefIdx . txInInfoOutRef) tidx
  where
    go :: (Eq b) => (TxInInfo -> b) -> b -> Maybe TxOutRef
    go f x =
        case filter (\(f -> y) -> y == x) ins of
            [] -> Nothing
            (r : _) -> return $ txInInfoOutRef r

{- | Builds @ScriptContext@ according to given configuration and
 @SpendingBuilder@.

 This function will yield @Nothing@ when the builder was never given a
 validator input--from @inputFromValidator@ or
 @inputFromValidatorWith@.

 @since 1.1.0
-}
buildSpending ::
    SpendingBuilder ->
    Maybe ScriptContext
buildSpending builder@(unpack -> BB{..}) = do
    vInIden <- sbValidatorInput builder
    let (ins, inDat) = yieldInInfoDatums bbInputs builder
        (outs, outDat) = yieldOutDatums bbOutputs
        mintedValue = yieldMint bbMints
        extraDat = yieldExtraDatums bbDatums
        base = yieldBaseTxInfo builder
        txinfo =
            base
                { txInfoInputs = ins
                , txInfoOutputs = outs
                , txInfoData = fromList $ inDat <> outDat <> extraDat
                , txInfoMint = mintedValue
                , txInfoSignatories = toList $ bbSignatures
                }
    vInRef <- yieldValidatorInput ins vInIden
    Just $ ScriptContext txinfo (Spending vInRef)

{- | Check builder with provided checker, then build spending context.

 @since 2.1.0
-}
checkBuildSpending :: Checker SpendingBuilder -> SpendingBuilder -> Either String ScriptContext
checkBuildSpending checker builder =
    case toList (runChecker checker builder) of
        [] -> buildSpending builder
        err -> Left $ show err

-- | Builds spending context; it throwing error when builder fails.
buildSpendingUnsafe ::
    SpendingBuilder ->
    ScriptContext
buildSpendingUnsafe = fromJust . buildSpending
