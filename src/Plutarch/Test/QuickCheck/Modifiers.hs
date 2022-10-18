{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}

module Plutarch.Test.QuickCheck.Modifiers (
  -- * Types
  AdaSymbolPresence (..),
  GenCurrencySymbol (..),
  GenValue (..),
) where

import Control.Monad (guard)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.List (nub, sort)
import Data.Word (Word8)
import GHC.Exts (fromList, fromListN, toList)
import PlutusLedgerApi.V2 (
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
  Value (Value),
  fromBuiltin,
  toBuiltin,
 )
import qualified PlutusTx.AssocMap as AssocMap
import Test.QuickCheck (
  ASCIIString (ASCIIString),
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  Gen,
  shrinkList,
 )
import Test.QuickCheck.Function (functionMap)
import qualified Test.QuickCheck.Gen as Gen

{- | Type-level marker to indicate whether a 'GenCurrencySymbol' can have an ADA
 'CurrencySymbol' inside it or not.

 @since 2.1.3
-}
data AdaSymbolPresence
  = WithAdaSymbol
  | WithoutAdaSymbol
  deriving stock
    ( -- | @since 2.1.3
      Eq
    , -- | @since 2.1.3
      Show
    , -- | @since 2.1.3
      Ord
    )

{- | A helper (opaque) newtype for QuickCheck use with 'CurrencySymbol's. Has a
 type-level tag to indicate whether or not it could potentially contain a
 'CurrencySymbol'. We provide instances of 'Arbitrary', 'CoArbitrary' and
 'Function' around this newtype, intended to act on the 'CurrencySymbol' inside
 it.

 The easiest way to use this newtype is by pattern matching:

 > forAll arbitrary $ \(GenCurrencySymbol @WithAdaSymbol sym) -> ...

 You can also \'re-wrap\' for shrinking:

 > shrink $ GenCurrencySymbol @WithAdaSymbol sym

However, as 'GenCurrencySymbol' instances do not shrink, there's not much point.

 @since 2.1.3
-}
newtype GenCurrencySymbol (p :: AdaSymbolPresence)
  = GenCurrencySymbol CurrencySymbol
  deriving
    ( -- | @since 2.1.3
      Eq
    )
    via CurrencySymbol
  deriving stock
    ( -- | @since 2.1.3
      Show
    )

-- Ensures we can't accidentally mess around with 'coerce'
type role GenCurrencySymbol nominal

{- | This instance occasionally has the ability to produce the ADA symbol (that
 is, the empty 'CurrencySymbol'). However, this is weighted quite heavily in
 favour of the ADA symbol: in theory, assuming every byte is equally probable
 (a safe assumption, since 'CurrencySymbol's are in general hashes), the odds
 of getting the ADA symbol should be

 \[
 frac{1}{2^{8 \cdot 28 = 224} + 1}
 \]

 However, in this case, the odds are actually

 \[
 \frac{1}{2^{62} + 1}
 \]

 This is a limitation of probability distributions as implemented by
 QuickCheck, which uses 'Int' for this purpose. Keep this in mind when using
 this generator: if you're unsure, it's better to \'seed\' the
 'WithoutAdaSymbol' instance to a probability you're happy with using
 'Gen.frequency'.

 This instance does not shrink, as it makes very little sense to.

 @since 2.1.3
-}
instance Arbitrary (GenCurrencySymbol 'WithAdaSymbol) where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    Gen.frequency
      [ (1, pure . GenCurrencySymbol . CurrencySymbol . toBuiltin @ByteString $ "")
      , (big, coerce <$> arbitrary @(GenCurrencySymbol 'WithoutAdaSymbol))
      ]
    where
      -- 2^62
      big :: Int
      big = 4_611_686_018_427_387_904

{- | This instance never produces the ADA symbol. Like the corresponding
 'WithAdaSymbol' instance, this instance does not shrink.

 @since 2.1.3
-}
instance Arbitrary (GenCurrencySymbol 'WithoutAdaSymbol) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    inner <- fromListN 28 <$> Gen.vectorOf 28 arbitrary
    pure
      . GenCurrencySymbol
      . CurrencySymbol
      . toBuiltin @ByteString
      $ inner

-- | @since 2.1.3
instance CoArbitrary (GenCurrencySymbol p) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (GenCurrencySymbol (CurrencySymbol inner)) =
    coarbitrary (toList . fromBuiltin $ inner)

-- | @since 2.1.3
instance Function (GenCurrencySymbol p) where
  {-# INLINEABLE function #-}
  function =
    functionMap
      (\(GenCurrencySymbol (CurrencySymbol inner)) -> toList . fromBuiltin $ inner)
      (GenCurrencySymbol . CurrencySymbol . toBuiltin @ByteString . fromList)

{- | A helper (opaque) newtype for QuickCheck use with 'Value's. Has two type
 arguments, to indicate, in order, what kind of amounts ADA and non-ADA
 entries are allowed to have.

 The 'Value's wrapped inside this type are phase-1 valid; the only validity we
 can't guarantee is to do with amounts, as these depend on choice of
 modifiers.

 The easiest way to use this newtype is by pattern matching:

 > forAll arbitrary $ \(GenValue @NonNegative @Positive val) -> ...

 In this case, the ADA entry in @val@ would contain a non-negative amount, but
 any other entry would contain a strictly positive one.

 You can also \'re-wrap\' for shrinking:

 > shrink $ GenValue @NonNegative @Positive val

 = Note

 As 'GenValue' relies heavily on 'Coercible' to work, the newtype constructors
 of your modifiers need to be in scope, or you will get strange errors. For
 example, if you want to do

 > GenValue @NonNegative @Positive val <- arbitrary

 you must have both the 'NonNegative' and 'Positive' types, as well as their
 newtype constructors, imported.

 @since 2.1.3
-}
newtype GenValue (adaMod :: Type -> Type) (otherMod :: Type -> Type)
  = GenValue Value
  deriving
    ( -- | @since 2.1.3
      Eq
    )
    via Value
  deriving stock
    ( -- | @since 2.1.3
      Show
    )

-- similar coerce shenanigan prevention
type role GenValue nominal nominal

{- | This instance ensures phase-1 validity up to amounts. Specifically, the
 following are guaranteed to hold irrespective of \'tag choices\':

 - Entries whose 'CurrencySymbol' and 'TokenName' match are unique.
 - \'Outer\' map entries are sorted by 'CurrencySymbol'.
 - \'Inner\' map entries are sorted by 'TokenName'.
 - An ADA entry always exists: this corresponds to a mapping from the
 'CurrencySymbol' @""@ to the 'TokenName' @""@ and an amount.
 - ADA entries have singleton \'inner maps\'.
 - Non-ADA entries have non-empty \'inner maps\'.

 The kind of amount generated for ADA and non-ADA entries is controlled by the
 two tags: the first determines how the ADA amount will get generated, while
 the second determines how any non-ADA amounts get generated. Thus,
 @GenValue 'NonNegative 'Positive@ will mean the ADA entry has a zero or
 positive amount, but any other entry will be strictly positive.

 In order to ensure all invariants hold, the shrinker for 'GenValue' can only
 perform the following on the underlying 'Value':

 - Remove non-ADA \'outer map\' entries;
 - Remove \'inner map\' entries; and
 - Shrink the amounts associated with an entry according to the tags.

 More specifically, neither 'CurrencySymbol' or 'TokenMap' keys will be
 shrunk.

 @since 2.1.3
-}
instance
  ( Arbitrary (adaMod Integer)
  , Arbitrary (otherMod Integer)
  , forall (a :: Type). Coercible (adaMod a) a
  , forall (a :: Type). Coercible (otherMod a) a
  ) =>
  Arbitrary (GenValue adaMod otherMod)
  where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    GenValue . Value . AssocMap.fromList <$> do
      -- First, make an ADA entry
      adaEntry <- mkAdaEntry @adaMod
      -- Then, generate the rest, making sure not to use the ADA CurrencySymbol
      syms <- mkOtherEntrySymbols
      (adaEntry :) <$> traverse go syms
    where
      go ::
        CurrencySymbol ->
        Gen (CurrencySymbol, AssocMap.Map TokenName Integer)
      go sym =
        (sym,) . AssocMap.fromList <$> do
          tokNames <- mkTokenNames
          traverse (pairWith @otherMod) tokNames
  {-# INLINEABLE shrink #-}
  shrink (GenValue (Value inner)) = case AssocMap.toList inner of
    -- This case is technically impossible, as we never 'shrink away' the ADA
    -- entry.
    [] -> error "Shrinker for GenValue: Empty 'outer map'."
    (adaEntry : otherEntries) ->
      GenValue . Value . AssocMap.fromList <$> do
        -- Handle ADA entry shrinks by only shrinking the sole amount it has in
        -- it.
        case adaEntry of
          ("", adaInner) -> case AssocMap.toList adaInner of
            [("", adaMod)] -> do
              adaMod' <- coerciveShrink @adaMod adaMod
              let adaEntry' = ("", AssocMap.fromList [("", adaMod')])
              -- Shrink whatever remains according to the rules.
              otherEntries' <-
                shrinkList (traverse (go . AssocMap.toList)) otherEntries
              -- Mash everything together.
              pure $ adaEntry' : otherEntries'
            _ -> error "Shrinker for GenValue: Malformed 'inner map' for ADA entry."
          _ -> error "Shrinker for GenValue: Bad CurrencySymbol for ADA entry."
    where
      go :: [(TokenName, Integer)] -> [AssocMap.Map TokenName Integer]
      go kvs = do
        kvs' <- shrinkList (traverse (coerciveShrink @otherMod)) kvs
        guard (not . null $ kvs')
        pure . AssocMap.fromList $ kvs'

-- | @since 2.1.3
instance CoArbitrary (GenValue adaMod otherMod) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary x = coarbitrary (unwrap x)

-- | @since 2.1.3
instance Function (GenValue adaMod otherMod) where
  {-# INLINEABLE function #-}
  function = functionMap unwrap rewrap

-- Helpers

unwrap ::
  forall (adaMod :: Type -> Type) (otherMod :: Type -> Type).
  GenValue adaMod otherMod ->
  [([Word8], [([Word8], Integer)])]
unwrap (GenValue (Value inner)) =
  bimap unwrapCS unwrapInner <$> AssocMap.toList inner
  where
    unwrapCS :: CurrencySymbol -> [Word8]
    unwrapCS (CurrencySymbol bbs) = toList . fromBuiltin $ bbs
    unwrapInner ::
      AssocMap.Map TokenName Integer ->
      [([Word8], Integer)]
    unwrapInner innerMap =
      first (\(TokenName tn) -> toList . fromBuiltin $ tn) <$> AssocMap.toList innerMap

rewrap ::
  forall (adaMod :: Type -> Type) (otherMod :: Type -> Type).
  [([Word8], [([Word8], Integer)])] ->
  GenValue adaMod otherMod
rewrap =
  GenValue
    . Value
    . AssocMap.fromList
    . fmap (bimap rewrapCS rewrapInner)
  where
    rewrapCS :: [Word8] -> CurrencySymbol
    rewrapCS = CurrencySymbol . toBuiltin @ByteString . fromList
    rewrapInner :: [([Word8], Integer)] -> AssocMap.Map TokenName Integer
    rewrapInner =
      AssocMap.fromList
        . fmap (first (TokenName . toBuiltin @ByteString . fromList))

mkAdaEntry ::
  forall (adaMod :: Type -> Type).
  ( forall (a :: Type). Coercible (adaMod a) a
  , Arbitrary (adaMod Integer)
  ) =>
  Gen (CurrencySymbol, AssocMap.Map TokenName Integer)
mkAdaEntry = do
  amount <- coerce @(adaMod Integer) @Integer <$> arbitrary
  pure ("", AssocMap.fromList [("", amount)])

mkOtherEntrySymbols :: Gen [CurrencySymbol]
mkOtherEntrySymbols =
  sort . nub . coerce <$> Gen.listOf (arbitrary @(GenCurrencySymbol 'WithoutAdaSymbol))

mkTokenNames :: Gen [TokenName]
mkTokenNames = do
  x <- gen
  xs <- Gen.listOf gen
  pure . sort . nub $ x : xs
  where
    gen :: Gen TokenName
    gen =
      TokenName <$> do
        ASCIIString name <- arbitrary
        pure
          . toBuiltin @ByteString
          . fromList
          . fmap (fromIntegral . ord)
          $ name

pairWith ::
  forall (otherMod :: Type -> Type).
  ( Arbitrary (otherMod Integer)
  , forall (a :: Type). Coercible (otherMod a) a
  ) =>
  TokenName ->
  Gen (TokenName, Integer)
pairWith tn = (tn,) <$> coerciveArbitrary @otherMod

coerciveArbitrary ::
  forall (mod :: Type -> Type).
  ( Coercible (mod Integer) Integer
  , Arbitrary (mod Integer)
  ) =>
  Gen Integer
coerciveArbitrary = coerce @(mod Integer) @Integer <$> arbitrary

coerciveShrink ::
  forall (mod :: Type -> Type).
  ( Coercible (mod Integer) Integer
  , Arbitrary (mod Integer)
  ) =>
  Integer ->
  [Integer]
coerciveShrink =
  fmap (coerce @(mod Integer) @Integer)
    . shrink
    . coerce @Integer @(mod Integer)
