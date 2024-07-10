{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
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
  MintingBuilder,

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
import Optics (A_Lens, LabelOptic (labelOptic), lens, set, view)
import Plutarch.Context.Base (
  BaseBuilder,
  Builder (pack, _bb),
  mintToValue,
  unpack,
  yieldBaseTxInfo,
  yieldExtraDatums,
  yieldInInfoDatums,
  yieldMint,
  yieldOutDatums,
  yieldRedeemerMap,
 )
import Plutarch.Context.Check (
  Checker,
  CheckerError,
  CheckerErrorType (OtherError),
  checkBool,
  checkFail,
  flattenValue,
  handleErrors,
  runChecker,
 )
import Plutarch.Context.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  TxInfo (
    txInfoDCert,
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoRedeemers,
    txInfoReferenceInputs,
    txInfoSignatories,
    txInfoWdrl
  ),
  Value,
 )
import Prettyprinter qualified as P (Pretty (pretty))
import PlutusTx.AssocMap qualified as AssocMap

{- | A context builder for Minting. Corresponds to
 'Plutus.V1.Ledger.Contexts.Minting' specifically.

 @since 2.5.0
-}
data MintingBuilder = MB BaseBuilder (Maybe CurrencySymbol)
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ BaseBuilder, b ~ BaseBuilder) =>
  LabelOptic "inner" k MintingBuilder MintingBuilder a b
  where
  labelOptic = lens (\(MB x _) -> x) $ \(MB _ cs) inner' -> MB inner' cs

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Maybe CurrencySymbol, b ~ Maybe CurrencySymbol) =>
  LabelOptic "mintingCS" k MintingBuilder MintingBuilder a b
  where
  labelOptic = lens (\(MB _ x) -> x) $ \(MB inner _) cs' -> MB inner cs'

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
  _bb = #inner
  pack x = set #inner x (mempty :: MintingBuilder)

instance Normalizer MintingBuilder where
  mkNormalized' (MB bb cs) =
    MB (mkNormalized bb) cs

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
buildMinting' builder@(unpack -> bb) =
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
          , txInfoData = AssocMap.unsafeFromList $ inDat <> outDat <> extraDat
          , txInfoMint = mintedValue
          , txInfoRedeemers = AssocMap.unsafeFromList $ toList (view #redeemers bb) <> redeemerMap
          , txInfoSignatories = toList . view #signatures $ bb
          , txInfoWdrl = AssocMap.unsafeFromList $ toList (view #withdrawals bb)
          , txInfoDCert = toList (view #dcerts bb)
          }
      mintcs = case view #mintingCS builder of
        Just cs ->
          if hasCS mintedValue cs
            then Minting cs
            else Minting ""
        Nothing -> Minting ""
   in ScriptContext txinfo mintcs

{- | Check builder with provided checker, then build minting context.

 @since 2.1.0
-}
buildMinting :: [Checker MintingError MintingBuilder] -> MintingBuilder -> ScriptContext
buildMinting c = buildMinting' . handleErrors (mconcat c <> checkMinting)

{- | Same as `buildMinting` but instead of throwing error it returns `Either`.

 @since 2.1.0
-}
tryBuildMinting :: Checker MintingError MintingBuilder -> MintingBuilder -> Either [CheckerError MintingError] ScriptContext
tryBuildMinting c b = case toList $ runChecker (c <> checkMinting) b of
  [] -> Right $ buildMinting' b
  errs -> Left errs

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
    ((foldMap mintToValue . toList . view #mints . unpack) &&& view #mintingCS)
    ( choose
        (\(mints, cs) -> maybe (Left ()) (Right . hasCS mints) cs)
        (checkFail $ OtherError MintingCurrencySymbolNotGiven)
        (checkBool $ OtherError MintingCurrencySymbolNotFound)
    )

hasCS :: Value -> CurrencySymbol -> Bool
hasCS val cs = any (\(x, _, _) -> x == cs) . flattenValue $ val
