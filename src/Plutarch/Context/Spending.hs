{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.Spending
 Copyright: (C) Liqwid Labs 2022
 License: Proprietary
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
    withSpending,

    -- * builder
    buildSpending,
    buildSpendingUnsafe,
) where

import Control.Monad.Cont (ContT (runContT), MonadTrans (lift))
import Data.Foldable (Foldable (toList))
import Plutarch.Context.Base -- (
--    BaseBuilder (bbDatums, bbInputs, bbMints, bbOutputs, bbSignatures),
--    Builder (..),
--    UTXO,
--    scriptUTXOGeneral,
--    scriptUTXOGeneralWith,
--    yieldBaseTxInfo,
--    yieldExtraDatums,
--    yieldInInfoDatums,
--    yieldMint,
--    yieldOutDatums,
-- )

import PlutusLedgerApi.V1 (Credential (..))
import PlutusLedgerApi.V1.Contexts (
    ScriptContext (ScriptContext),
    ScriptPurpose (Spending),
    TxInfo (
        txInfoData,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories
    ),
    txInInfoOutRef,
    txInInfoResolved,
 )

{- | A context builder for spending. Corresponds broadly to validators, and to
 'PlutusLedgerApi.V1.Contexts.Spending' specifically.

 @since 1.0.0
-}
data SpendingBuilder = SB
    { sbInner :: BaseBuilder
    , sbValidatorInput :: Maybe UTXO
    }
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

-- | @since 1.1.0
instance Builder SpendingBuilder where
    pack = flip SB Nothing
    unpack = sbInner

-- | @since 1.0.0
instance Semigroup SpendingBuilder where
    SB inner _ <> SB inner' (Just vin') =
        SB (inner <> inner') $ Just vin'
    SB inner vin <> SB inner' Nothing =
        SB (inner <> inner') vin

-- | @since 1.1.0
instance Monoid SpendingBuilder where
    mempty = SB mempty Nothing

withSpending ::
    (UTXO -> UTXO) ->
    SpendingBuilder
withSpending f =
    mempty
        { sbValidatorInput = Just $ f (UTXO (PubKeyCredential "") mempty Nothing Nothing Nothing)
        }

{- | Builds @ScriptContext@ according to given configuration and
 @SpendingBuilder@.

 This function will yield @Nothing@ when the builder was never given a
 validator input--from @inputFromValidator@ or
 @inputFromValidatorWith@.

 @since 1.1.0
-}
buildSpending ::
    SpendingBuilder ->
    Either String ScriptContext
buildSpending builder = flip runContT Right $
    case sbValidatorInput builder of
        Nothing -> lift $ Left "No validator input specified"
        Just vInUTXO -> do
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
                spendingInInfo = filter (\(txInInfoResolved -> x) -> x == utxoToTxOut vInUTXO) ins

            case spendingInInfo of
                [] -> lift $ Left "UTXO Input not found"
                (x : _) -> return $ ScriptContext txinfo (Spending (txInInfoOutRef x))

-- | Builds spending context; it throwing error when builder fails.
buildSpendingUnsafe ::
    SpendingBuilder ->
    ScriptContext
buildSpendingUnsafe = either error id . buildSpending  
