{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.Minting
 Copyright: (C) Liqwid Labs 2022
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

    -- * Input
    withMinting,

    -- * builder
    buildMinting,
    buildMintingUnsafe,
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
    flattenValue,
 )

{- | A context builder for Minting. Corresponds to
 'Plutus.V1.Ledger.Contexts.Minting' specifically.

 @since 1.0.0
-}
data MintingBuilder = MB
    { mbInner :: BaseBuilder
    , mbMintingCS :: Maybe CurrencySymbol
    }
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

-- | @since 1.1.0
instance Semigroup MintingBuilder where
    MB inner _ <> MB inner' cs@(Just _) =
        MB (inner <> inner') $ cs
    MB inner cs <> MB inner' Nothing =
        MB (inner <> inner') cs

-- | @since 1.1.0
instance Monoid MintingBuilder where
    mempty = MB mempty Nothing

-- | @since 1.1.0
instance Builder MintingBuilder where
    pack = flip MB Nothing
    unpack = mbInner

withMinting :: CurrencySymbol -> MintingBuilder -> MintingBuilder
withMinting cs (MB inner _) = MB inner $ Just cs

{- | Builds @ScriptContext@ according to given configuration and
 @MintingBuilder@.

 This function will yield @Nothing@ when @mint@ interface was never
 called with a non-empty value, or when the specified currency symbol
 @CurrencySymbol@ cannot be found.

 @since 1.1.0
-}
buildMinting ::
    MintingBuilder ->
    Either String ScriptContext
buildMinting builder = flip runContT Right $
    case mbMintingCS builder of
        Nothing -> lift $ Left "No minting currency symbol specified"
        Just mintingCS -> do
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
                mintingInfo =
                    filter
                        (\(cs, _, _) -> cs == mintingCS)
                        $ flattenValue mintedValue

            case mintingInfo of
                [] -> lift $ Left "Minting CS not found"
                _ -> return $ ScriptContext txinfo (Minting mintingCS)

-- | Builds minting context; it throwing error when builder fails.
buildMintingUnsafe ::
    MintingBuilder ->
    ScriptContext
buildMintingUnsafe = either error id . buildMinting
