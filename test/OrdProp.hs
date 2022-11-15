{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}

module OrdProp (tests) where

import Plutarch.Extra.Maybe (pisJust, ptraceIfNothing)
import Plutarch.Extra.Ord (
  PComparator,
  pallUniqueBy,
  pfromOrd,
  pisSortedBy,
  pnubSortBy,
  preverseComparator,
  psortBy,
 )
import Plutarch.Test.QuickCheck (PA, TestableTerm (TestableTerm), fromPFun)
import Test.QuickCheck (
  Property,
  arbitrary,
  forAllShrinkShow,
  scale,
  shrink,
 )
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

tests :: TestTree
tests =
  adjustOption go $
    testGroup
      "Plutarch.Extra.Ord"
      [ testProperty "sorted lists should prove sorted" propSortedList
      , testProperty "singleton lists are always sorted" propSortedSingleton
      , testProperty "nubbed lists should prove ordered" propNubList
      , testProperty "nubbed lists should prove unique" propNubList'
      , testProperty "singleton lists are always nubbed" propNubSingleton
      ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 1000

-- Properties

propSortedSingleton :: Property
propSortedSingleton =
  forAllShrinkShow arbitrary shrink show . fromPFun $
    plam $ \x ->
      pisSortedBy # cmp #$ psortBy @_ @PList # cmp #$ psingleton # x

propSortedList :: Property
propSortedList =
  forAllShrinkShow (scale (`div` 6) arbitrary) shrink showLen . fromPFun $
    plam $ \xs ->
      pisSortedBy # cmp #$ psortBy @_ @PList # cmp # xs

propNubList :: Property
propNubList =
  forAllShrinkShow (scale (`div` 4) arbitrary) shrink showLen . fromPFun $
    plam $ \xs ->
      pisJust #$ pallUniqueBy # cmp #$ pnubSortBy @_ @PList # cmp # xs

propNubList' :: Property
propNubList' =
  forAllShrinkShow (scale (`div` 4) arbitrary) shrink showLen . fromPFun $
    plam $ \xs ->
      ptraceIfNothing
        "unexpectedly out-of-order"
        (pallUniqueBy # cmp #$ pnubSortBy @_ @PList # cmp # xs)

propNubSingleton :: Property
propNubSingleton =
  forAllShrinkShow arbitrary shrink show . fromPFun $
    plam $ \x ->
      ptraceIfNothing
        "unexpectedly out-of-order"
        (pallUniqueBy # cmp #$ pnubSortBy @_ @PList # cmp #$ psingleton # x)

-- Helpers

cmp :: forall (s :: S). Term s (PComparator PA)
cmp = preverseComparator # pfromOrd

showLen :: TestableTerm (PList PA) -> String
showLen xs@(TestableTerm ell) =
  "contents: "
    <> show xs
    <> "\nlength: "
    <> show (TestableTerm $ plength # ell)
