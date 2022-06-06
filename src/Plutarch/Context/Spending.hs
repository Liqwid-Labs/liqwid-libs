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
    inputFromValidatorWith,
    inputFromValidator,

    -- * builder
    buildSpending,
) where

import Control.Monad.Cont (ContT (runContT), MonadTrans (lift))
import Data.Foldable (Foldable (toList))
import Data.Kind (Type)
import Plutarch (S)
import Plutarch.Builtin (PIsData)
import Plutarch.Context.Base (
    BaseBuilder (bbDatums, bbInputs, bbMints, bbOutputs, bbSignatures),
    Builder (..),
    UTXO,
    scriptUTXOGeneral,
    scriptUTXOGeneralWith,
    yieldBaseTxInfo,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldMint,
    yieldOutDatums,
 )
import Plutarch.Context.Config (ContextConfig, configTxId)
import Plutarch.Lift (PUnsafeLiftDecl (..))
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
    TxOutRef (TxOutRef),
 )
import PlutusLedgerApi.V1.Scripts (ValidatorHash)
import PlutusLedgerApi.V1.Value (Value)

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

{- | Updates the input of the validator that is being validated.
 This function will override what the previous "input from validator,"
 if one exists.

 @since 1.1.0
-}
inputFromValidator ::
    ValidatorHash ->
    Value ->
    SpendingBuilder
inputFromValidator vh val =
    mempty
        { sbValidatorInput = Just $ scriptUTXOGeneral vh val
        }

{- | Equivalent to @inputFromValidator@ but with Datum.

 @since 1.1.0
-}
inputFromValidatorWith ::
    forall (d :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ d, PIsData p) =>
    ValidatorHash ->
    Value ->
    d ->
    SpendingBuilder
inputFromValidatorWith vh val d =
    mempty
        { sbValidatorInput = Just $ scriptUTXOGeneralWith vh val d
        }

{- | Builds @ScriptContext@ according to given configuration and
 @SpendingBuilder@.

 This function will yield @Nothing@ when the builder was never given a
 validator input--from @inputFromValidator@ or
 @inputFromValidatorWith@.

 @since 1.1.0
-}
buildSpending ::
    ContextConfig ->
    SpendingBuilder ->
    Maybe ScriptContext
buildSpending config builder = flip runContT Just $
    case sbValidatorInput builder of
        Nothing -> lift Nothing
        Just (pure -> vInUTXO) -> do
            let bb = unpack builder

            (ins, inDat) <- yieldInInfoDatums (bbInputs bb <> vInUTXO) config
            (outs, outDat) <- yieldOutDatums (bbOutputs bb)
            mintedValue <- yieldMint (bbMints bb)
            extraDat <- yieldExtraDatums (bbDatums bb)
            base <- yieldBaseTxInfo config

            let txinfo =
                    base
                        { txInfoInputs = ins
                        , txInfoOutputs = outs
                        , txInfoData = inDat <> outDat <> extraDat
                        , txInfoMint = mintedValue
                        , txInfoSignatories = toList $ bbSignatures bb
                        }

            return $ ScriptContext txinfo (Spending (TxOutRef (configTxId config) $ toInteger (length ins)))
