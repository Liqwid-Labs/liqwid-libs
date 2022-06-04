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
    SpendingBuilder (..),
    buildSpending,
    inputFromValidatorWith,
    inputFromValidator,
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
    baseTxInfo,
    scriptUTXOGeneral,
    scriptUTXOGeneralWith,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldMint,
    yieldOutDatums,
 )
import Plutarch.Context.Config (ContextConfig)
import Plutarch.Lift (PUnsafeLiftDecl (..))
import PlutusLedgerApi.V1.Address (scriptHashAddress)
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
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (Datum, DatumHash, ValidatorHash)
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

instance Builder SpendingBuilder where
    pack = flip SB Nothing
    unpack = sbInner

-- Avoids any coercion funny business
type role SpendingBuilder

-- | @since 1.0.0
instance Semigroup SpendingBuilder where
    SB inner _ <> SB inner' (Just vin') =
        SB (inner <> inner') $ Just vin'
    SB inner vin <> SB inner' Nothing =
        SB (inner <> inner') vin

instance Monoid SpendingBuilder where
    mempty = SB mempty Nothing

inputFromValidator ::
    ValidatorHash ->
    Value ->
    SpendingBuilder
inputFromValidator vh val =
    mempty
        { sbValidatorInput = Just $ scriptUTXOGeneral vh val
        }

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
            base <- baseTxInfo config

            let txinfo =
                    base
                        { txInfoInputs = ins
                        , txInfoOutputs = outs
                        , txInfoData = inDat <> outDat <> extraDat
                        , txInfoMint = mintedValue
                        , txInfoSignatories = toList $ bbSignatures bb
                        }

            return $ ScriptContext txinfo (Spending (TxOutRef "" $ toInteger (length ins)))
