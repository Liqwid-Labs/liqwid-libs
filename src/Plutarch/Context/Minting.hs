{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.Minting
 Copyright: (C) Liqwid Labs 2022
 License: Proprietary
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Builder for minting contexts. 'MintingBuilder' is an instance of 'Semigroup',
 which allows combining the results of this API's functions into a larger
 'MintingBuilder' using '<>'.
-}
module Plutarch.Context.Minting (
    -- * Types
    MintingBuilder (..),

    -- * builder
    buildMinting,
) where

import Control.Monad.Cont (ContT (runContT))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Foldable (Foldable (toList))
import Plutarch.Context.Base (
    BaseBuilder (bbDatums, bbInputs, bbMints, bbOutputs, bbSignatures),
    Builder (..),
    yieldBaseTxInfo,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldMint,
    yieldOutDatums,
 )

import PlutusLedgerApi.V1.Contexts (
    ScriptContext (ScriptContext),
    ScriptPurpose (Minting),
    TxInfo (
        txInfoData,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories
    ),
 )
import PlutusLedgerApi.V1.Value (
    CurrencySymbol,
    Value,
    symbols,
 )

{- | A context builder for Minting. Corresponds to
 'Plutus.V1.Ledger.Contexts.Minting' specifically.

 @since 1.0.0
-}
data MintingBuilder = MB
    { mbInner :: BaseBuilder
    }
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

-- | @since 1.1.0
instance Semigroup MintingBuilder where
    MB inner <> MB inner' =
        MB (inner <> inner')

-- | @since 1.1.0
instance Monoid MintingBuilder where
    mempty = MB mempty

-- | @since 1.1.0
instance Builder MintingBuilder where
    pack = MB
    unpack = mbInner

uniformCurrencySymbol ::
    Value ->
    Maybe CurrencySymbol
uniformCurrencySymbol (symbols -> xs)
    | length xs == 1 = Just $ head xs
    | otherwise = Nothing

{- | Builds @ScriptContext@ according to given configuration and
 @MintingBuilder@.

 This function will yield @Nothing@ when @mint@ interface was never
 called with a non-empty value or values contain a multiple
 @CurrencySymbol@.

 @since 1.1.0
-}
buildMinting ::
    MintingBuilder ->
    Either String ScriptContext
buildMinting builder = flip runContT Right $
    do
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

        case uniformCurrencySymbol mintedValue of
            Just c -> return $ ScriptContext txinfo (Minting c)
            Nothing -> lift $ Left "Duplicate currency symbol or No minted value"
