{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Monad (replicateM)
import Control.Monad.Loops (iterateUntilM)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (find, sort)
import Data.Maybe (isJust)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Benchmark.Exhaustive (cardSets, exhGenBags, exhGenSets)
import Plutarch.Benchmark.Sized (Cardinality (..))
import Test.QuickCheck (Gen, chooseInt, discard, expectFailure, forAll, sized, (===))
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckMaxSize, QuickCheckTests, testProperty)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain . adjustOption go . testGroup "Properties" $
    [ exhaustiveProps
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10000

exhaustiveProps :: TestTree
exhaustiveProps =
  adjustOption (min (5 :: QuickCheckMaxSize)) $
    testGroup
      "Exhaustive"
      [ testProperty "exhGenBags random containment" $
          let gen = sized $ \size -> do
                let range = testRange size
                    choice = uncurry enumFromTo range
                    pick n = replicateM n (chooseInt range)
                bag <- sort <$> pick size
                pure (size, choice, bag)
              prop (size, choice, bag) =
                let bags = exhGenBags size choice
                 in isJust $ find (== bag) bags
           in forAll gen prop
      , testProperty "exhGenSets random containment" $
          let gen = sized $ \size -> do
                let range = testRange size
                    choice = uncurry enumFromTo range
                set <- IntSet.toList <$> genIntSet range size
                pure (size, choice, set)
              prop (size, choice, set) =
                let sets = exhGenSets size choice
                 in isJust $ find (== set) sets
           in forAll gen prop
      , testProperty "exhGenBags over-demand" $
          expectFailure $
            seq (exhGenBags 1 []) ()
      , testProperty "exhGenSets over-demand" $
          expectFailure $
            seq (exhGenSets 1 []) ()
      , testProperty "exhGenBags zero" $
          exhGenBags 0 [1 :: Int] === [[]]
      , testProperty "exhGenSets zero" $
          exhGenSets 0 [1 :: Int] === [[]]
      , testProperty "exhGenSets length" $
          let gen = sized $ \size -> do
                let range = testRange size
                    choice = uncurry enumFromTo range
                pure (size, choice)
              prop (size, choice) =
                let sets = exhGenSets size choice
                    (lo, hi) = testRange size
                    expectedCard = cardSets (hi - lo + 1) size
                 in case expectedCard of
                      HugeCardinality -> discard
                      Cardinality card -> length sets === fromIntegral card
           in forAll gen prop
      ]

-- | range used for testing
testRange :: Int -> (Int, Int)
testRange size = (1, size * 2)

-- | range, size -> set
genIntSet :: (Int, Int) -> Int -> Gen IntSet
genIntSet range size = do
  let genElem = chooseInt range
      expand s = genElem >>= \e -> pure $ IntSet.insert e s
  iterateUntilM (\s -> IntSet.size s == size) expand IntSet.empty
