{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
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
import Test.QuickCheck (Property, arbitrary, forAllShrinkShow, scale, shrink)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain . adjustOption go . testGroup "Properties" $
        [ testGroup
            "Plutarch.Extra.Ord"
            [ testProperty "sorted lists should prove sorted" propSortedList
            , testProperty "nubbed lists should prove ordered" propNubList
            , testProperty "nubbed lists should prove unique" propNubList'
            ]
        ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 1000

-- Properties

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

-- Helpers

cmp :: forall (s :: S). Term s (PComparator PA)
cmp = preverseComparator # pfromOrd

showLen :: TestableTerm (PList PA) -> String
showLen xs@(TestableTerm ell) =
    "contents: "
        <> show xs
        <> "\nlength: "
        <> show (TestableTerm $ plength # ell)
