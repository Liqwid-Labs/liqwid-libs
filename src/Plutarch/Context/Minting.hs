{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
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
    checkBuildMinting,
    buildMintingUnsafe,
) where

import Data.Foldable (Foldable (toList))
import Data.Maybe (fromJust)
import Plutarch.Context.Base (
    BaseBuilder (
        BB,
        bbDatums,
        bbInputs,
        bbMints,
        bbOutputs,
        bbSignatures
    ),
    Builder (..),
    yieldBaseTxInfo,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldMint,
    yieldOutDatums,
 )
import PlutusLedgerApi.V2 (
    CurrencySymbol,
    ScriptContext (ScriptContext),
    ScriptPurpose (Minting),
    TxInfo (
        txInfoData,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories
    ),
    Value (getValue),
    fromList,
 )
import qualified PlutusTx.AssocMap as Map (toList)

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
        MB (inner <> inner') cs
    MB inner cs <> MB inner' Nothing =
        MB (inner <> inner') cs

-- | @since 1.1.0
instance Monoid MintingBuilder where
    mempty = MB mempty Nothing

-- | @since 1.1.0
instance Builder MintingBuilder where
    pack = flip MB Nothing
    unpack = mbInner

{- | Set CurrencySymbol for building Minting ScriptContext.

 @since 1.1.1
-}
withMinting :: CurrencySymbol -> MintingBuilder
withMinting cs = MB mempty $ Just cs

{- | Builds @ScriptContext@ according to given configuration and
 @MintingBuilder@.

 This function will yield @Nothing@ when @mint@ interface was never
 called with a non-empty value, or when the specified currency symbol
 @CurrencySymbol@ cannot be found.

 @since 1.1.0
-}
buildMinting ::
    MintingBuilder ->
    Maybe ScriptContext
buildMinting builder@(unpack -> BB{..}) = do
    mintingCS <- mbMintingCS builder
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
        mintingInfo =
            filter
                (\(cs, _) -> cs == mintingCS)
                $ Map.toList $ getValue mintedValue

    case mintingInfo of
        [] -> Nothing
        _ -> Just $ ScriptContext txinfo (Minting mintingCS)

{- | Check builder with provided checker, then build minting context.

 @since 2.1.0
-}
checkBuildMinting :: Checker MintingBuilder -> MintingBuilder -> Either String ScriptContext
checkBuildMinting checker builder =
    case toList (runChecker checker builder) of
        [] -> buildMinting builder
        err -> Left $ show err

-- | Builds minting context; it throwing error when builder fails.
buildMintingUnsafe ::
    MintingBuilder ->
    ScriptContext
buildMintingUnsafe = fromJust . buildMinting
