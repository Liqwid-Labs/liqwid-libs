{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Spec.Extra.List (tests) where

--------------------------------------------------------------------------------

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (
    Arbitrary (arbitrary),
    Property,
    Testable (property),
    elements,
    forAll,
    suchThat,
    testProperty,
    (.&&.),
 )

--------------------------------------------------------------------------------

import Data.List (nub, sort, sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (last)

--------------------------------------------------------------------------------

import Plutarch (PMatch (pmatch), Term, phoistAcyclic, plam, (#), (#$), type (:-->))
import Plutarch.Api.V1 (PMaybeData (..))
import Plutarch.Bool (PBool, PEq ((#==)), (#<))
import Plutarch.Builtin (PBuiltinList, PBuiltinPair, pfstBuiltin, psndBuiltin)
import Plutarch.Extra.Functor (pfmap)
import Plutarch.Extra.Maybe (pmaybeToMaybeData)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant, plift)
import Plutarch.Pair (PPair (..))

--------------------------------------------------------------------------------

import Control.Monad.Cont (cont, runCont)

--------------------------------------------------------------------------------

import Plutarch.Extra.List (pbinarySearchBy, phalve, pisUniq, pmergeBy, pmsort, pnubSort)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testProperty "'pmsort' sorts a list properly" prop_msortCorrect
    , testProperty "'pmerge' merges two sorted lists into one sorted list" prop_mergeCorrect
    , testProperty "'phalve' splits a list in half as expected" prop_halveCorrect
    , testProperty "'pnubSort' sorts a list and remove duplicate elements" prop_nubSortProperly
    , testProperty "'pisUniq' can tell whether all elements in a list are unique" prop_uniqueList
    , testProperty "'pbinarySearch' searches a target in a sorted list as expected" prop_binarySerchCorrect
    ]

--------------------------------------------------------------------------------
-- Prperties.

-- | Yield true if 'pmsort' sorts a given list correctly.
prop_msortCorrect :: [Integer] -> Bool
prop_msortCorrect l = sorted == expected
  where
    -- Expected sorted list, using 'Data.List.sort'.
    expected :: [Integer]
    expected = sort l

    --

    psorted :: Term s (PBuiltinList PInteger)
    psorted = pmsort # pconstant l

    sorted :: [Integer]
    sorted = plift psorted

-- | Yield true if 'pmerge' merges two list into a ordered list correctly.
prop_mergeCorrect :: [Integer] -> [Integer] -> Bool
prop_mergeCorrect a b = merged == expected
  where
    -- Sorted list a and b
    sa = sort a
    sb = sort b

    -- Merge two lists which are assumed to be ordered.
    merge :: [Integer] -> [Integer] -> [Integer]
    merge xs [] = xs
    merge [] ys = ys
    merge sx@(x : xs) sy@(y : ys)
        | x <= y = x : merge xs sy
        | otherwise = y : merge sx ys

    expected :: [Integer]
    expected = merge sa sb

    --

    pmerged :: Term _ (PBuiltinList PInteger)
    pmerged = pmergeBy # plam (#<) # pconstant sa # pconstant sb

    merged :: [Integer]
    merged = plift pmerged

{- | Yield true if Plutarch level 'phalve' splits a given list
   as its Haskell level counterpart does.
-}
prop_halveCorrect :: [Integer] -> Bool
prop_halveCorrect l = halved == expected
  where
    -- Halve a list.
    halve :: [Integer] -> ([Integer], [Integer])
    halve xs' = go xs' xs'
      where
        go xs [] = ([], xs)
        go (x : xs) [_] = ([x], xs)
        go (x : xs) (_ : _ : ys) =
            let (first, last) =
                    go xs ys
             in (x : first, last)
        go [] _ = ([], [])

    expected :: ([Integer], [Integer])
    expected = halve l

    --

    phalved :: Term _ (PPair (PBuiltinList PInteger) (PBuiltinList PInteger))
    phalved = phalve # pconstant l

    halved :: ([Integer], [Integer])
    halved =
        let f = plift $ pmatch phalved $ \(PPair x _) -> x
            s = plift $ pmatch phalved $ \(PPair _ x) -> x
         in (f, s)

{- | Yield true if 'pnubSort' sorts and removes
   duplicate elements from a given list.
-}
prop_nubSortProperly :: [Integer] -> Bool
prop_nubSortProperly l = nubbed == expected
  where
    -- Sort and list and then nub it.
    expected :: [Integer]
    expected = nub $ sort l

    --

    pnubbed :: Term _ (PBuiltinList PInteger)
    pnubbed = pnubSort # pconstant l

    nubbed :: [Integer]
    nubbed = plift pnubbed

{- | Yield true if 'isUnique' can correctly determine
   whether a given list only contains unique elements or not.
-}
prop_uniqueList :: [Integer] -> Bool
prop_uniqueList l = isUnique == expected
  where
    -- Convert input list to a set.
    -- If the set's size equals to list's size,
    --   the list only contains unique elements.
    expected :: Bool
    expected = S.size (S.fromList l) == length l

    --

    isUnique = plift $ pisUniq # pconstant l

-- | Test that 'pbinarySearchBy' works as expected.
prop_binarySerchCorrect :: Property
prop_binarySerchCorrect =
    runCont
        ( do
            -- Generate a bunch unique keys.
            keys <-
                cont $
                    forAll $
                        arbitrary @(S.Set Integer) `suchThat` (not . S.null)

            -- Generate key-value pairs.
            kvPairs <- cont $ forAll $ mapM (\k -> (k,) <$> (arbitrary @Integer)) $ S.toList keys

            (targetKey, _) <- cont $ forAll $ elements kvPairs

            nonexistentKey <-
                cont $
                    forAll $
                        arbitrary @Integer `suchThat` (\k -> not $ S.member k keys)

            pure
                ( property (isPlutarchVersionCorrect kvPairs targetKey)
                    .&&. property (isPlutarchVersionCorrect kvPairs nonexistentKey)
                )
        )
        id
  where
    isPlutarchVersionCorrect :: [(Integer, Integer)] -> Integer -> Bool
    isPlutarchVersionCorrect pairs target = native pairs target == plutarch pairs target

    native :: [(Integer, Integer)] -> Integer -> Maybe Integer
    native pairs key = M.lookup key $ M.fromList pairs

    plutarch :: [(Integer, Integer)] -> Integer -> Maybe Integer
    plutarch pairs target = plift result
      where
        sortedPairs = sortBy (\(a, _) (b, _) -> a `compare` b) pairs

        ppairs :: Term _ (PBuiltinList (PBuiltinPair PInteger PInteger))
        ppairs = pconstant sortedPairs

        ptarget :: Term _ PInteger
        ptarget = pconstant target

        result :: Term _ (PMaybeData PInteger)
        result = phoistAcyclic $ pmaybeToMaybeData # result'
          where
            comp :: Term _ (PBuiltinPair PInteger PInteger :--> PInteger :--> PBool)
            comp = phoistAcyclic $ plam $ \((pfstBuiltin #) -> x) y -> x #< y

            eq :: Term _ (PBuiltinPair PInteger PInteger :--> PInteger :--> PBool)
            eq = phoistAcyclic $ plam $ \((pfstBuiltin #) -> x) y -> x #== y

            result' =
                pfmap # plam (\((psndBuiltin #) -> x) -> x)
                    #$ pbinarySearchBy # eq # comp # ptarget # ppairs
