{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
    buildMinting',
    buildMinting,
    tryBuildMinting,
    checkMinting,
) where

import Control.Arrow ((&&&))
import Data.Foldable (Foldable (toList))
import Data.Functor.Contravariant (contramap)
import Data.Functor.Contravariant.Divisible (choose)
import Optics (lens)
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
    unpack,
    yieldBaseTxInfo,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldMint,
    yieldOutDatums,
 )
import Plutarch.Context.Check (
    Checker (runChecker),
    CheckerError,
    CheckerErrorType (OtherError),
    checkBool,
    checkFail,
    flattenValue,
    handleErrors,
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
    Value,
    fromList,
 )
import qualified Prettyprinter as P (Pretty (pretty))

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
    _bb = lens mbInner (\x b -> x{mbInner = b})
    pack x = mempty{mbInner = x}

{- | Set CurrencySymbol for building Minting ScriptContext.

 @since 1.1.1
-}
withMinting :: CurrencySymbol -> MintingBuilder
withMinting cs = MB mempty $ Just cs

{- | Builds @ScriptContext@ according to given configuration and
 @MintingBuilder@.

 @since 2.1.0
-}
buildMinting' ::
    MintingBuilder ->
    ScriptContext
buildMinting' builder@(unpack -> BB{..}) =
    let (ins, inDat) = yieldInInfoDatums bbInputs
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

        mintcs = case mbMintingCS builder of
            Just cs ->
                if hasCS mintedValue cs
                    then Minting cs
                    else Minting ""
            Nothing -> Minting ""
     in ScriptContext txinfo mintcs

{- | Check builder with provided checker, then build minting context.

 @since 2.1.0
-}
buildMinting :: Checker MintingError MintingBuilder -> MintingBuilder -> ScriptContext
buildMinting c = buildMinting' . handleErrors (c <> checkMinting)

{- | Same as `buildMinting` but instead of throwing error it returns `Either`.

 @since 2.1.0
-}
tryBuildMinting :: Checker MintingError MintingBuilder -> MintingBuilder -> Either [CheckerError MintingError] ScriptContext
tryBuildMinting c b = case toList $ runChecker (c <> checkMinting) b of
    [] -> Right $ buildMinting' b
    errs -> Left $ errs

-- | @since 2.1.0
data MintingError
    = MintingCurrencySymbolNotGiven
    | MintingCurrencySymbolNotFound
    deriving stock (Show)

-- | @since 2.1.0
instance P.Pretty MintingError where
    pretty MintingCurrencySymbolNotGiven = "Minting Currency Symbol is not given"
    pretty MintingCurrencySymbolNotFound = "Specified Currency Symbol is not found on mints"

-- | @since 2.1.0
checkMinting :: Checker MintingError MintingBuilder
checkMinting =
    contramap
        ((mconcat . toList . bbMints . unpack) &&& mbMintingCS)
        ( choose
            (\(mints, cs) -> maybe (Left ()) (Right . (hasCS mints)) cs)
            (checkFail $ OtherError MintingCurrencySymbolNotGiven)
            (checkBool $ OtherError MintingCurrencySymbolNotFound)
        )

hasCS :: Value -> CurrencySymbol -> Bool
hasCS val cs = not . null . filter (\(x, _, _) -> x == cs) . flattenValue $ val
