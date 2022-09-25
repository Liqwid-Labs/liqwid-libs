{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Spec.Extra.List (tests) where

--------------------------------------------------------------------------------

import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Property,
  listOf1,
  orderedList,
  suchThat,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Plutarch.Property (classifiedPropertyNative, peqPropertyNative')
import Test.Tasty.QuickCheck (
  testProperty,
 )

--------------------------------------------------------------------------------

import Control.Monad (join)
import Data.List (nub, sort)
import qualified Data.Set as S
import Data.Universe (Finite, Universe)
import qualified GHC.Generics as GHC
import Prelude hiding (last)

--------------------------------------------------------------------------------

import Plutarch.Extra.List (pisSorted, pisUniq, pmergeBy, pmsort, pnubSort)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
  [ testProperty "'pmsort' sorts a list properly" prop_msortCorrect
  , testProperty "'pmerge' merges two sorted lists into one sorted list" prop_mergeCorrect
  , testProperty "'pnubSort' sorts a list and remove duplicate elements" prop_nubSortProperly
  , testProperty "'pisUniq' can tell whether all elements in a list are unique" prop_determineIsUniqueListCorrect
  , testProperty "'pisSorted' can distinguish sorted and unsorted list" prop_determineIsListSortedCorrect
  ]

--------------------------------------------------------------------------------

-- | The property of `pmsort` to make a list sorted in ascending order.
prop_msortCorrect :: Property
prop_msortCorrect = peqPropertyNative' expected arbitrary shrink pmsort
  where
    expected :: [Integer] -> [Integer]
    expected = sort

-- | The property of 'pmergeBy' to merge two ordered list into one ordered list.
prop_mergeCorrect :: Property
prop_mergeCorrect = peqPropertyNative' expected generator shrink definition
  where
    generator :: Gen ([Integer], [Integer])
    generator = (,) <$> orderedList <*> orderedList

    expected :: ([Integer], [Integer]) -> [Integer]
    expected = uncurry merge

    -- Merge two lists which are assumed to be in ascending ordered.
    merge :: [Integer] -> [Integer] -> [Integer]
    merge xs [] = xs
    merge [] ys = ys
    merge sx@(x : xs) sy@(y : ys)
      | x <= y = x : merge xs sy
      | otherwise = y : merge sx ys

    definition ::
      Term
        _
        ( PBuiltinPair
            (PBuiltinList PInteger)
            (PBuiltinList PInteger)
            :--> PBuiltinList PInteger
        )
    definition = phoistAcyclic $
      plam $ \pair ->
        let a = pfstBuiltin # pair
            b = psndBuiltin # pair
            lt = phoistAcyclic $ plam (#<)
         in pmergeBy # lt # a # b

-- | The property of 'pnubSort'to sort and removes duplicate elements from a given list.
prop_nubSortProperly :: Property
prop_nubSortProperly = peqPropertyNative' expected arbitrary shrink pnubSort
  where
    expected :: [Integer] -> [Integer]
    expected = nub . sort

{- | The property of 'isUnique' to correctly determine whether
  a given list only contains unique elements or not.
-}
prop_determineIsUniqueListCorrect :: Property
prop_determineIsUniqueListCorrect = peqPropertyNative' expected arbitrary shrink pisUniq
  where
    -- Convert input list to a set.
    -- If the set's size equals to list's size,
    --   the list only contains unique elements.
    expected :: [Integer] -> Bool
    expected l = S.size (S.fromList l) == length l

data ListSortedCase
  = SortedList
  | UnsortedList
  deriving stock (GHC.Generic)
  deriving stock (Show, Enum, Bounded, Eq)
  deriving anyclass (Universe, Finite)

prop_determineIsListSortedCorrect :: Property
prop_determineIsListSortedCorrect =
  classifiedPropertyNative generator shrink expected classifier pisSorted
  where
    genNotSorted :: (Ord a, Arbitrary a) => Gen [a]
    genNotSorted = join <$> listOf1 genUnorderedSeq
      where
        genUnorderedSeq = do
          a <- arbitrary
          b <- arbitrary `suchThat` (> a)
          notMid <-
            arbitrary
              `suchThat` (\x -> not $ a <= x && x <= b)
          return [a, notMid, b]

    generator :: ListSortedCase -> Gen [Integer]
    generator SortedList = orderedList
    generator UnsortedList = genNotSorted

    expected :: [Integer] -> Maybe Bool
    expected l = Just $ sort l == l

    classifier :: [Integer] -> ListSortedCase
    classifier l =
      if sort l == l
        then SortedList
        else UnsortedList
