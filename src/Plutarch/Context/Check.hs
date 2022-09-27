{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Context.Check (
  Checker (..),
  CheckerErrorType (..),
  CheckerPos (..),
  CheckerError (..),
  flattenValue,
  renderErrors,
  handleErrors,
  basicError,
  checkAt,
  checkFoldable,
  checkIf,
  checkIfWith,
  checkFail,
  checkBool,
  checkWith,
  checkBSLength,
  checkPositiveValue,
  checkTxId,
  checkSignatures,
  checkZeroSum,
  checkInputs,
  checkReferenceInputs,
  checkMints,
  checkFee,
  checkOutputs,
  checkDatumPairs,
  checkPhase1,
  checkNormalized,
  checkValidatorRedeemer,
  checkValueNormalized,
) where

import Acc (Acc)
import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Decidable (choose, lose), Divisible (conquer, divide))
import Data.Maybe (fromMaybe)
import Data.Void (absurd)
import Plutarch.Context.Base (
  BaseBuilder (..),
  Builder,
  UTXO (..),
  mintToValue,
  normalizeMint,
  normalizeValue,
  unpack,
 )
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol,
  LedgerBytes (LedgerBytes),
  PubKeyHash (PubKeyHash),
  TokenName,
  ValidatorHash (ValidatorHash),
  Value (Value, getValue),
  adaSymbol,
  adaToken,
  getPubKeyHash,
  getTxId,
 )
import PlutusTx.AssocMap qualified as AssocMap (mapMaybe, toList)
import PlutusTx.Builtins (lengthOfByteString)
import Prettyprinter qualified as P

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
  | NonNormalizedValue Value
  | NoZeroSum Value
  | MissingRedeemer ValidatorHash
  | SpecifyRedeemerForNonValidatorInput
  | OtherError e
  deriving stock (Show, Eq)

{- | Possible positions for errors from phase-1 checker

 @since 2.1.0
-}
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

-- | @since 2.1.0
newtype CheckerError e
  = CheckerError (CheckerErrorType e, CheckerPos)
  deriving stock (Show)

-- | @since 2.1.0
instance P.Pretty e => P.Pretty (CheckerErrorType e) where
  pretty (IncorrectByteString lb) = "\"" <> P.pretty lb <> "\"" P.<+> "is an invalid bytestring"
  pretty NoSignature = "Signature not provided"
  pretty OrphanDatum = "Extra datum is provided"
  pretty (MintingAda val) = "Transaction mints Ada:" P.<+> P.pretty val
  pretty (NonAdaFee val) = "Transaction takes tokens other than Ada as fee:" P.<+> P.pretty val
  pretty (DuplicateTxOutRefIndex idx) = "Overlapping input indices: " P.<+> P.pretty idx
  pretty (NonPositiveValue val) = P.pretty val P.<+> "is an invalid value (contains non positive)."
  pretty (NonNormalizedValue val) = P.pretty val P.<+> "is not normalized."
  pretty (NoZeroSum val) =
    "Transaction doesn't not have equal inflow and outflow." <> P.line
      <> "Diff:" P.<+> P.pretty val
  pretty (MissingRedeemer hash) =
    "Missing script redeemer while spending UTxO owned by:" P.<+> P.pretty hash
  pretty SpecifyRedeemerForNonValidatorInput =
    "Set redeemer for a input that is not owned by a validator"
  pretty (OtherError e) = P.pretty e

-- | @since 2.1.0
instance P.Pretty CheckerPos where
  pretty = P.pretty . drop 2 . show

-- | @since 2.1.0
instance P.Pretty e => P.Pretty (CheckerError e) where
  pretty (CheckerError (err, at)) =
    "Error at" P.<+> P.pretty at <> ":"
      <> P.line
      <> P.indent 4 (P.pretty err)

{- | Checker that accumulates error.

 @since 2.1.0
-}
newtype Checker e a = Checker {runChecker :: a -> Acc (CheckerError e)}

-- | @since 2.1.0
instance Contravariant (Checker e) where
  contramap f (Checker x) = Checker $ \y -> x . f $ y

-- | @since 2.1.0
instance Divisible (Checker e) where
  conquer = Checker $ const mempty
  divide f x y = Checker $ \(f -> (x', y')) -> runChecker x x' <> runChecker y y'

-- | @since 2.1.0
instance Decidable (Checker e) where
  lose f = Checker $ \a -> absurd $ f a
  choose f x y = Checker $ \(f -> c) ->
    case c of
      Left x' -> runChecker x x'
      Right y' -> runChecker y y'

-- | @since 2.1.0
instance Semigroup (Checker e a) where
  f <> g = Checker $ \y -> runChecker f y <> runChecker g y

-- | @since 2.1.0
instance Monoid (Checker e a) where
  mempty = Checker $ const mempty

{- | Render and prettified list of errors.

 @since 2.1.0
-}
renderErrors :: (Foldable t, P.Pretty e) => t (CheckerError e) -> String
renderErrors err = show . P.indent 4 $ P.line <> P.vsep (P.pretty <$> toList err)

{- | Check type @a@ with given checker. It returns input
 if there are no errors; throws error if not.

 @since 2.1.0
-}
handleErrors :: P.Pretty e => Checker e a -> a -> a
handleErrors checker x
  | null errs = x
  | otherwise = error $ renderErrors errs
  where
    errs = runChecker checker x

{- | Construct `CheckerError` from `CheckerErrorType` with default
 position and as singleton.

 @since 2.1.0
-}
basicError :: CheckerErrorType e -> Acc (CheckerError e)
basicError err = pure $ CheckerError (err, AtTxInfo)

{- | Check that always fails with given error.

 @since 2.1.0
-}
checkFail :: CheckerErrorType e -> Checker e a
checkFail = Checker . const . basicError

{- | Update/Override checker position.

 @since 2.1.0
-}
checkAt :: CheckerPos -> Checker e a -> Checker e a
checkAt at c = Checker (fmap (updatePos at) . runChecker c)
  where
    updatePos at' (CheckerError (x, _)) = CheckerError (x, at')

{- | Apply checker type @a@ to foldable @t a@. It will check all
 elements of the foldable structure.

 @since 2.1.0
-}
checkFoldable :: Foldable t => Checker e a -> Checker e (t a)
checkFoldable c = Checker $ \y -> foldMap (runChecker c) y

{- | Build checker with a predicate.

 @since 2.1.0
-}
checkIf :: (a -> Bool) -> CheckerErrorType e -> Checker e a
checkIf f err = Checker $ \y ->
  if f y
    then mempty
    else basicError err

{- | Build checker that checks @Bool@.

 @since 2.1.0
-}
checkBool :: CheckerErrorType e -> Checker e Bool
checkBool = checkIf id

{- | Build checker via CPS computation. This allows to pick up
 what is given to checker and use that externally.

=== Example:
@
checkWith $ \x ->
        contramap
            (all (\(cs, tk, _) -> cs /= adaSymbol && tk /= adaToken) . flattenValue)
            (checkBool $ MintingAda x)
@
It is especially useful when one needs to provide error some specific information.

 @since 2.1.0
-}
checkWith :: (a -> Checker e a) -> Checker e a
checkWith x = Checker $ \y -> runChecker (x y) y

{- | Combination of `checkIf` and `checkWith`.

 @since 2.1.0
-}
checkIfWith :: (a -> Bool) -> (a -> CheckerErrorType e) -> Checker e a
checkIfWith f err = Checker $ \y ->
  if f y
    then mempty
    else basicError $ err y

{- | Verify on-chain bytestring if it matches the given length

 @since 2.1.0
-}
checkBSLength :: Int -> Checker e BuiltinByteString
checkBSLength len = checkWith $ \x -> contramap lengthOfByteString $ checkIf (== toInteger len) (IncorrectByteString $ LedgerBytes x)

{- | Check if all tokens in `Value` are positive.

 @since 2.1.0
-}
checkPositiveValue :: Checker e Value
checkPositiveValue = checkWith $ \x -> contramap isPos $ checkBool (NonPositiveValue x)
  where
    isPos = all (\(_, _, x) -> x > 0) . flattenValue

{- | Check if 'Value' is normalized.

 @since 2.4.0
-}
checkValueNormalized :: Checker e Value
checkValueNormalized =
  checkWith $ \x -> contramap isNormalized $ checkBool (NonNormalizedValue x)
  where
    isNormalized val = getValue (normalizeValue val) == getValue val

checkCredential :: Checker e Credential
checkCredential = contramap classif $ checkBSLength 28
  where
    classif (PubKeyCredential (PubKeyHash x)) = x
    classif (ScriptCredential (ValidatorHash x)) = x

{- | Check if a validator output has a redeemer attached.

 @since 2.3.0
-}
checkValidatorRedeemer :: Checker e UTXO
checkValidatorRedeemer =
  contramap
    ((fromMaybe (PubKeyCredential "") . utxoCredential) &&& utxoRedeemer)
    ( checkWith $ \case
        (ScriptCredential _, Just _) -> mempty
        (ScriptCredential h, Nothing) -> checkFail $ MissingRedeemer h
        (_, Just _) -> checkFail SpecifyRedeemerForNonValidatorInput
        _ -> mempty
    )

{- | Check if TxId follows the format

 @since 2.1.0
-}
checkTxId :: Builder a => Checker e a
checkTxId =
  checkAt AtTxId $
    contramap (getTxId . bbTxId . unpack) (checkBSLength 32)

{- | Check if atleast one signature exists and all follows the format.

 @since 2.1.0
-}
checkSignatures :: Builder a => Checker e a
checkSignatures =
  checkAt AtSignatories $
    mconcat
      [ contramap (fmap getPubKeyHash . bbSignatures . unpack) (checkFoldable $ checkBSLength 28)
      , contramap (length . bbSignatures . unpack) (checkIf (>= 1) NoSignature)
      ]

{- | Check if input, output, mint have zero sum.

 @since 2.1.0
-}
checkZeroSum :: Builder a => Checker e a
checkZeroSum = Checker $
  \(unpack -> BB {..}) ->
    let diff x (Value y) = x <> Value (AssocMap.mapMaybe (Just . AssocMap.mapMaybe (Just . negate)) y)
        -- TODO: This is quite wired, AssocMap doesn't implment Functor, but it does on haddock.
        i = mconcat . toList $ utxoValue <$> bbInputs
        o = mconcat . toList $ utxoValue <$> bbOutputs
        m = foldMap mintToValue . toList $ bbMints
     in if i <> m /= o <> bbFee
          then basicError $ NoZeroSum (diff (i <> m <> bbFee) o)
          else mempty

{- | Check if all input UTXOs follow format and have TxOutRef.

 @since 2.1.0
-}
checkInputs :: Builder a => Checker e a
checkInputs =
  mconcat
    [ checkAt AtInput $
        mconcat
          [ contramap
              (fmap utxoValue . bbInputs . unpack)
              (checkFoldable checkPositiveValue)
          , contramap
              (fmap (fromMaybe (PubKeyCredential "") . utxoCredential) . bbInputs . unpack)
              (checkFoldable checkCredential)
          , contramap
              (bbInputs . unpack)
              (checkFoldable checkValidatorRedeemer)
          ]
    , checkAt AtInputOutRef $
        mconcat
          [ contramap -- TODO: we should have `checkMaybe` here.
              (fmap (getTxId . fromMaybe "" . utxoTxId) . bbInputs . unpack)
              (checkFoldable $ checkBSLength 28)
          , contramap
              (getDups . toList . fmap (fromMaybe 0 . utxoTxIdx) . bbInputs . unpack)
              (checkWith $ const $ checkIfWith null DuplicateTxOutRefIndex)
          ]
    ]
  where
    getDups :: Eq a => [a] -> [a]
    getDups (x : xs)
      | x `elem` xs = if x `elem` dups then dups else x : dups
      | otherwise = dups
      where
        dups = getDups xs
    getDups [] = []

{- | Check if all reference input UTXOs follow format.

 @since 2.1.0
-}
checkReferenceInputs :: Builder a => Checker e a
checkReferenceInputs =
  checkAt AtReferenceInput $
    mconcat
      [ contramap
          (fmap (fromMaybe (PubKeyCredential "") . utxoCredential) . bbReferenceInputs . unpack)
          (checkFoldable checkCredential)
      , contramap
          (fmap utxoValue . bbReferenceInputs . unpack)
          (checkFoldable checkPositiveValue)
      ]

{- | Check if minted tokens are valid.

 @since 2.1.0
-}
checkMints :: Builder a => Checker e a
checkMints =
  checkAt AtMint $
    contramap (foldMap mintToValue . toList . bbMints . unpack) nullAda
  where
    nullAda :: Checker e Value
    nullAda = checkWith $ \x ->
      contramap
        (all (\(cs, tk, _) -> cs /= adaSymbol && tk /= adaToken) . flattenValue)
        (checkBool $ MintingAda x)

{- | Check if fee amount is valid.

 @since 2.1.0
-}
checkFee :: Builder a => Checker e a
checkFee =
  checkAt AtFee $
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
checkOutputs =
  checkAt AtOutput $
    mconcat
      [ contramap (fmap utxoValue . bbOutputs . unpack) (checkFoldable checkPositiveValue)
      , contramap
          (fmap (fromMaybe (PubKeyCredential "") . utxoCredential) . bbOutputs . unpack)
          (checkFoldable checkCredential)
      ]

{- | Check if builder does not provide excess datum.

 @since 2.1.0
-}
checkDatumPairs :: Builder a => Checker e a
checkDatumPairs =
  checkAt AtData $
    contramap (length . bbDatums . unpack) (checkIf (== 0) OrphanDatum)

{- | Check if values in builder are normalized.

 @since 2.4.0
-}
checkNormalized :: Builder a => Checker e a
checkNormalized =
  mconcat
    [ checkAt AtOutput $
        contramap (fmap utxoValue . bbOutputs . unpack) (checkFoldable checkValueNormalized)
    , checkAt AtInput $
        contramap (fmap utxoValue . bbInputs . unpack) (checkFoldable checkValueNormalized)
    , checkAt AtReferenceInput $
        contramap (fmap utxoValue . bbReferenceInputs . unpack) (checkFoldable checkValueNormalized)
    , checkAt AtMint $
        contramap
          (bbMints . unpack)
          ( checkFoldable $
              checkWith $ \x ->
                checkIf (\m -> m == normalizeMint m) $ NonNormalizedValue (mintToValue x)
          )
    ]

{- | All checks combined for Phase-1 check.

 @since 2.1.0
-}
checkPhase1 :: Builder a => [Checker e a]
checkPhase1 =
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

{- | Flatten value into tuple of `CurrencySymbol`, `TokenName`, and `Integer`.

 @since 2.1.0
-}
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
flattenValue x = concatMap (\(cs, z) -> (\(tk, amt) -> (cs, tk, amt)) <$> AssocMap.toList z) (AssocMap.toList (getValue x))
