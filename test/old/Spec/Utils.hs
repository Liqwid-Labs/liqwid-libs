{-# LANGUAGE ViewPatterns #-}

module Spec.Utils (
  genKeySet,
  genSortedMapWithKeys,
  genUnsortedMapWithKeys,
  genNonIdenticalKeySets,
  shrinkMap,
  genSingletonValue,
  sortMap,
  sortValue,
  genValue,
) where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Char8 as C (ByteString, pack)
import Data.ByteString.Hash (sha2_256)
import Data.List (sortBy)
import qualified Data.Set as S
import PlutusLedgerApi.V1 (
  BuiltinByteString,
  CurrencySymbol (..),
  TokenName (..),
  Value (..),
  toBuiltin,
 )
import PlutusLedgerApi.V1.Value (AssetClass (..))
import qualified PlutusLedgerApi.V1.Value as Value
import qualified PlutusTx.AssocMap as AssocMap
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
  chooseAny,
  listOf,
  oneof,
  shuffle,
  suchThat,
 )

-- | Generate a set of unique keys.
genKeySet :: (Ord k, Arbitrary k) => Gen (S.Set k)
genKeySet = arbitrary

-- | Generate a map given a key set.
genSortedMapWithKeys :: (Arbitrary v) => S.Set k -> Gen (AssocMap.Map k v)
genSortedMapWithKeys keys =
  AssocMap.fromList <$> mapM (\k -> (k,) <$> arbitrary) (S.toAscList keys)

-- | Generate an unsorted map given a key set.
genUnsortedMapWithKeys :: (Arbitrary v) => S.Set k -> Gen (AssocMap.Map k v)
genUnsortedMapWithKeys keys = do
  kl <- shuffle $ S.toList keys
  kvs <-
    mapM
      (\k -> (k,) <$> arbitrary)
      kl
  return $ AssocMap.fromList kvs

-- | Generate a pair of unidentical key sets.
genNonIdenticalKeySets ::
  (Ord k, Arbitrary k) =>
  Gen (S.Set k, S.Set k)
genNonIdenticalKeySets = do
  a <- genKeySet
  b <- genKeySet `suchThat` (/= a)
  return (a, b)

-- |c Shrinker for associative maps.
shrinkMap :: (Arbitrary k, Arbitrary v) => AssocMap.Map k v -> [AssocMap.Map k v]
shrinkMap (AssocMap.toList -> l) = AssocMap.fromList <$> shrink l

-- | Sort a map in ascending order.
sortMap :: (Ord k) => AssocMap.Map k v -> AssocMap.Map k v
sortMap (AssocMap.toList -> l) =
  AssocMap.fromList $ sortBy (\(ka, _) (kb, _) -> compare ka kb) l

{- | Generate a random Hash
Hashs cannot be shrunken; functions utilizing this function,
therefore, cannot be shrunken as well.

Note: copy-paste from https://github.com/Liqwid-Labs/agora/blob/main/agora-specs/Property/Generator.hs
-}
genHashByteString :: Gen C.ByteString
genHashByteString = sha2_256 . C.pack . show <$> (chooseAny @Integer)

-- | Generate a 'BuiltinByteString'.
genBuiltinByteString :: Gen BuiltinByteString
genBuiltinByteString = toBuiltin <$> genHashByteString

-- | Generate a valid 'CurrencySymbol'.
genCurrencySymbol :: Gen CurrencySymbol
genCurrencySymbol = CurrencySymbol <$> oneof [genBuiltinByteString, pure ""]

-- | Generate a valid 'TokenName'.
genTokenName :: Gen TokenName
genTokenName = TokenName <$> oneof [genBuiltinByteString, pure ""]

-- | Generate a valid 'AssetClass'.
genAssetClass :: Gen AssetClass
genAssetClass = AssetClass <$> ((,) <$> genCurrencySymbol <*> genTokenName)

-- | Generate a 'Value' that only contains positive amount of the given 'AssetClass'.
genSingletonValue :: Gen Value
genSingletonValue = Value.assetClassValue <$> genAssetClass <*> (arbitrary `suchThat` (> 0))

-- | Generate a 'Value'.
genValue :: Gen Value
genValue = mconcat <$> listOf genSingletonValue

-- | Sort a 'Value'. The inner tokename-amount map will be sorted as well.
sortValue :: Value -> Value
sortValue (Value.getValue -> inner) = Value $ sortMap sortedInner
  where
    sortedInner =
      AssocMap.fromList $
        map (Bifunctor.second sortMap) $
          AssocMap.toList inner
