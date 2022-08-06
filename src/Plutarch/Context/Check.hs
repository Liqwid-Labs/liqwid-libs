{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-all #-}

module Plutarch.Context.Check (
    Checker (..),
    CheckerErrorType (..),
    CheckerPos (..),
    checkPhase1,
    -- checkFoldable,
    -- checkIf,
    -- checkByteString,
    -- checkValue,
    -- checkTxId,
    -- checkSignatures,
    -- checkZeroSum,
    -- checkInputs,
    -- checkOutputs,
    -- checkDatumPairs,
    -- checkPhase1,
) where

import Acc (Acc)
import Data.Maybe (catMaybes)
import Data.Foldable (toList)
import Data.Functor ((<$))
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Divisible (divide, conquer))
import Plutarch.Context.Base (
    BaseBuilder (..),
    Builder,
    UTXO (..),
    unpack,
 )
import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Bytes
import qualified Prettyprinter as P
import qualified PlutusTx.AssocMap as AssocMap (mapMaybe, toList)
import PlutusTx.Builtins (lengthOfByteString)

{- | Possible errors from phase-1 checker

 @since 2.1.0
-}
data CheckerErrorType e
    = IncorrectByteString LedgerBytes
    | NoSignature 
    | OrphanDatum
    | MintingAda Value
    | NonAdaFee Value
    | DuplicateTxOutRefIndex [Integer]
    | NonPositiveValue Value
    | NoZeroSum Value
    | ErrorOther e
    deriving stock (Show, Eq)

data CheckerPos
    = AtInput
    | AtInputOutRef
    | AtReferenceInput
    | AtOutput
    | AtFee
    | AtMint
    | AtSignatories
    | AtData
    | AtTxId
    | AtTxInfo
    deriving stock (Show, Eq)

newtype CheckerError e
    = CheckerError (CheckerErrorType e, CheckerPos)
    deriving stock (Show)

instance P.Pretty e => P.Pretty (CheckerErrorType e) where
    pretty (IncorrectByteString lb) = "\"" <> P.pretty lb <> "\"" P.<+> "is an invalid bytestring"
    pretty NoSignature = "Signature not provided"
    pretty OrphanDatum = "Extra datum is provided"
    pretty (MintingAda val) = "Transaction mints Ada:" P.<+> P.pretty val
    pretty (NonAdaFee val) = "Transaction takes tokens other than Ada as fee:" P.<+> P.pretty val
    pretty (DuplicateTxOutRefIndex idx) = "Overlapping input indices: " P.<+> P.pretty idx
    pretty (NonPositiveValue val) = P.pretty val P.<+> "is an invalid value (contains non positive)."
    pretty (NoZeroSum val) = "Transaction doesn't not have equal inflow and outflow." <> P.line <>
                             "Diff:" P.<+> P.pretty val
    pretty (ErrorOther e) = P.pretty e

instance P.Pretty CheckerPos where
    pretty = P.pretty . drop 2 . show

instance P.Pretty e => P.Pretty (CheckerError e) where
    pretty (CheckerError (err, at)) =
        "Error at" P.<+> P.pretty at <> ":" <>
        P.line <> P.indent 4 (P.pretty err)
                                      
{- | Checker that accumulates error.

 @since 2.1.0
-}
newtype Checker e a = Checker {runChecker :: a -> Acc (CheckerError e)}

-- | @since 2.1.0
instance Contravariant (Checker e) where
    contramap f (Checker x) = Checker $ \y -> x . f $ y

-- | @since 2.1.0
instance Divisible (Checker e) where
    conquer = Checker $ \_ -> mempty
    divide f x y = Checker $ \(f -> (x', y')) -> (runChecker x x') <> (runChecker y y')

-- | @since 2.1.0
instance Semigroup (Checker e a) where
    f <> g = Checker $ \y -> runChecker f y <> runChecker g y

-- | @since 2.1.0
instance Monoid (Checker e a) where
    mempty = Checker $ const mempty

updatePos :: CheckerPos -> CheckerError a -> CheckerError a
updatePos at (CheckerError (x, _)) = CheckerError (x, at)

basicError :: CheckerErrorType e -> Acc (CheckerError e)
basicError err = pure $ CheckerError (err, AtTxInfo)

checkConst :: Bool -> CheckerErrorType e -> Checker e a
checkConst False err = Checker . const $ basicError err
checkConst True _ = mempty

checkAt :: CheckerPos -> Checker e a -> Checker e a
checkAt at c = Checker $
    \x -> updatePos at <$> runChecker c x

-- | @since 2.1.0
checkFoldable :: Foldable t => Checker e a -> Checker e (t a)
checkFoldable c = Checker $ \y -> foldMap (runChecker c) y

-- | @since 2.1.0
checkIf :: (a -> Bool) -> CheckerErrorType e -> Checker e a
checkIf f err = Checker $ \y ->
    if f y
    then mempty
    else basicError err

checkBool :: CheckerErrorType e -> Checker e Bool
checkBool err = checkIf id err

checkWith :: (a -> Checker e a) -> Checker e a
checkWith x = Checker $ \y -> runChecker (x y) y

checkIfWith :: (a -> Bool) -> (a -> CheckerErrorType e) -> Checker e a
checkIfWith f err = Checker $ \y ->
    if f y
    then mempty
    else basicError $ err y

-- | @since 2.1.0
checkByteString :: Checker e BuiltinByteString
checkByteString = checkWith $ \x -> contramap lengthOfByteString $ checkIf (== 28) (IncorrectByteString $ LedgerBytes x)

-- | @since 2.1.0
checkValue :: Checker e Value
checkValue = checkWith $ \x -> contramap isPos $ checkBool (NonPositiveValue x)
  where
    isPos = all (\(_, _, x) -> x > 0) . flattenValue

{- | Check if TxId follows the format

 @since 2.1.0
-}
checkTxId :: Builder a => Checker e a
checkTxId = checkAt AtTxId $ 
    contramap (getTxId . bbTxId . unpack) checkByteString

{- | Check if atleast one signature exists and all follows the format.

 @since 2.1.0
-}
checkSignatures :: Builder a => Checker e a
checkSignatures = checkAt AtSignatories $
    mconcat $
        [ contramap (fmap getPubKeyHash . bbSignatures . unpack) (checkFoldable $ checkByteString)
        , contramap (length . bbSignatures . unpack) (checkIf (>= 1) $ NoSignature)
        ]

{- | Check if input, output, mint have zero sum.

 @since 2.1.0
-}
checkZeroSum :: Builder a => Checker e a
checkZeroSum = Checker $
    \(unpack -> BB{..}) ->
        let diff x (Value y) = x <> Value (AssocMap.mapMaybe (Just . AssocMap.mapMaybe (Just . negate)) y)
            -- TODO: This is quite wired, AssocMap doesn't implment Functor, but it does on haddock.
            i = mconcat . toList $ utxoValue <$> bbInputs
            o = mconcat . toList $ utxoValue <$> bbOutputs
            m = mconcat . toList $ bbMints
         in if (i <> m /= o <> bbFee)
                then basicError $ NoZeroSum (diff (i <> m <> bbFee) o)
                else mempty

{- | Check if all input UTXOs follow format and have TxOutRef.

 @since 2.1.0
-}
checkInputs :: Builder a => Checker e a
checkInputs = 
    mconcat $
        [ checkAt AtInput $ contramap (fmap utxoValue . bbInputs . unpack)
            (checkFoldable $ checkValue)
        , checkAt AtInputOutRef $
            mconcat $
            [ contramap (fmap (getTxId . maybe "" id . utxoTxId) . bbInputs . unpack)
                (checkFoldable $ checkByteString)
            , contramap (getDups . catMaybes . toList . fmap utxoTxIdx . bbInputs . unpack)
                (checkWith $ \x -> checkIfWith null DuplicateTxOutRefIndex)
            ]
        ]
    where
      getDups :: Eq a => [a] -> [a]
      getDups (x: xs)
          | x `elem` xs = if x `elem` dups then dups else x:dups
          | otherwise = dups
          where
            dups = getDups xs
      getDups [] = []

checkReferenceInputs :: Builder a => Checker e a
checkReferenceInputs = checkAt AtReferenceInput $
    mconcat $
        [ contramap (fmap (getTxId . maybe "" id . utxoTxId) . bbReferenceInputs . unpack)
            (checkFoldable $ checkByteString)
        , contramap (fmap utxoValue . bbReferenceInputs . unpack)
            (checkFoldable $ checkValue)
        ]

checkMints :: Builder a => Checker e a
checkMints = checkAt AtMint $
    contramap (mconcat . toList . bbMints . unpack) nullAda
  where
    nullAda :: Checker e Value
    nullAda = checkWith $ \x ->
        contramap
          (all (\(cs, tk, _) -> cs /= adaSymbol && tk /= adaToken) . flattenValue)
          (checkBool $ MintingAda x)
        
checkFee :: Builder a => Checker e a
checkFee = checkAt AtFee $
    contramap (bbFee . unpack) onlyAda
  where
    onlyAda :: Checker e Value
    onlyAda = checkWith $ \x ->
        contramap
          (all (\(cs, tk, _) -> cs == adaSymbol && tk == adaToken) . flattenValue)
          (checkBool $ NonAdaFee x)
        
{- | Check if all output UTXOs follow format.

 @since 2.1.0
-}
checkOutputs :: Builder a => Checker e a
checkOutputs = checkAt AtOutput $
    contramap (fmap utxoValue . bbOutputs . unpack) (checkFoldable checkValue)

{- | Check if builder does not provide excess datum.

 @since 2.1.0
-}
checkDatumPairs :: Builder a => Checker e a
checkDatumPairs = checkAt AtData $
    contramap (length . bbDatums . unpack) (checkIf (== 0) OrphanDatum)

{- | All checks combined for Phase-1 check.

 @since 2.1.0
-}
checkPhase1 :: Builder a => Checker e a
checkPhase1 =
    mconcat $
        [ checkInputs
        , checkReferenceInputs
        , checkOutputs
        , checkDatumPairs
        , checkMints
        , checkFee
        , checkTxId
        , checkZeroSum
        , checkSignatures
        ]

flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
flattenValue x = concat $ (\(cs, z) -> (\(tk, amt) -> (cs, tk, amt)) <$> AssocMap.toList z) <$> AssocMap.toList (getValue x)
