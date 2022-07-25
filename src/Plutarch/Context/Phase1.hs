{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Context.Phase1 (
    Checker (..),
    CheckerError (..),
    checkFoldable,
    checkIf,
    checkByteString,
    checkValue,
    checkTxId,
    checkSignatures,
    checkZeroSum,
    checkInputs,
    checkOutputs,
    checkDatumPairs,
    checkPhase1,
) where

import Acc (Acc)
import Data.Foldable (toList)
import Data.Functor.Contravariant (Contravariant (contramap))
import Plutarch.Context.Base (
    BaseBuilder (..),
    Builder,
    UTXO (..),
    unpack,
 )
import PlutusLedgerApi.V1 (
    BuiltinByteString,
    PubKeyHash (getPubKeyHash),
    TxId (getTxId),
    Value (Value),
 )
import PlutusLedgerApi.V1.Value (flattenValue)
import qualified PlutusTx.AssocMap as AssocMap (mapMaybe)
import PlutusTx.Builtins (lengthOfByteString)

{- | Possible errors from phase-1 checker

 @since 2.1.0
-}
data CheckerError
    = IncorrectByteString
    | NoSignature
    | ExtraDatum
    | NonPositiveValue Value
    | NoZeroSum Value
    deriving stock (Show, Eq)

{- | Checker that accumulates error.

 @since 2.1.0
-}
newtype Checker a = Checker {runChecker :: a -> Acc CheckerError}

-- | @since 2.1.0
instance Contravariant Checker where
    contramap f (Checker x) = Checker $ \y -> x . f $ y

-- | @since 2.1.0
instance Semigroup (Checker a) where
    f <> g = Checker $ \y -> runChecker f y <> runChecker g y

-- | @since 2.1.0
instance Monoid (Checker a) where
    mempty = Checker $ const mempty

-- | @since 2.1.0
checkFoldable :: Foldable t => Checker a -> Checker (t a)
checkFoldable c = Checker $ \y -> foldMap (runChecker c) y

-- | @since 2.1.0
checkIf :: (a -> Bool) -> CheckerError -> Checker a
checkIf f err = Checker $ \y -> if f y then mempty else pure err

-- | @since 2.1.0
checkByteString :: Checker BuiltinByteString
checkByteString = Checker $ \y -> if lengthOfByteString y == 28 then mempty else pure $ IncorrectByteString

-- | @since 2.1.0
checkValue :: Checker Value
checkValue = Checker $ \y -> if isPos y then mempty else pure $ NonPositiveValue y
  where
    isPos = all (\(_, _, x) -> x > 0) . flattenValue

{- | Check if TxId follows the format

 @since 2.1.0
-}
checkTxId :: Builder a => Checker a
checkTxId = contramap (getTxId . bbTxId . unpack) checkByteString

{- | Check if atleast one signature exists and all follows the format.

 @since 2.1.0
-}
checkSignatures :: Builder a => Checker a
checkSignatures =
    mconcat $
        [ contramap (fmap getPubKeyHash . bbSignatures . unpack) (checkFoldable checkByteString)
        , contramap (length . bbSignatures . unpack) (checkIf (>= 1) NoSignature)
        ]

{- | Check if input, output, mint have zero sum.

 @since 2.1.0
-}
checkZeroSum :: Builder a => Checker a
checkZeroSum = Checker $
    \(unpack -> BB{..}) ->
        let diff x (Value y) = x <> Value (AssocMap.mapMaybe (Just . AssocMap.mapMaybe (Just . negate)) y)
            -- TODO: This is quite wired, AssocMap doesn't implment Functor, but it does on haddock.
            i = mconcat . toList $ utxoValue <$> bbInputs
            o = mconcat . toList $ utxoValue <$> bbOutputs
            m = mconcat . toList $ bbMints
         in if (i <> m /= o <> bbFee)
                then pure $ NoZeroSum (diff (i <> m <> bbFee) o)
                else mempty

{- | Check if all input UTXOs follow format and have TxOutRef.

 @since 2.1.0
-}
checkInputs :: Builder a => Checker a
checkInputs =
    mconcat $
        [ contramap (fmap (getTxId . maybe "" id . utxoTxId) . bbInputs . unpack) (checkFoldable checkByteString)
        , contramap (fmap utxoValue . bbInputs . unpack) (checkFoldable checkValue)
        ]

{- | Check if all output UTXOs follow format.

 @since 2.1.0
-}
checkOutputs :: Builder a => Checker a
checkOutputs =
    mconcat $
        [ contramap (fmap utxoValue . bbOutputs . unpack) (checkFoldable checkValue)
        ]

{- | Check if builder does not provide excess datum.

 @since 2.1.0
-}
checkDatumPairs :: Builder a => Checker a
checkDatumPairs = contramap (length . bbDatums . unpack) (checkIf (== 0) ExtraDatum)

{- | All checks combined for Phase-1 check.

 @since 2.1.0
-}
checkPhase1 :: Builder a => Checker a
checkPhase1 =
    mconcat $
        [ checkInputs
        , checkOutputs
        , checkDatumPairs
        , checkTxId
        , checkZeroSum
        , checkSignatures
        ]
