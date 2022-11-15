{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}

module Properties.NumericProp (tests) where

import Plutarch.Extra.Numeric ((#^))
import Test.QuickCheck (
  NonNegative (NonNegative),
  Property,
  (===),
 )
import Test.Tasty (adjustOption, testGroup, TestTree)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

tests :: TestTree
tests =
  adjustOption go $ testGroup "Numeric"
  [ testProperty "Integer power (#^)" propPowInt
  ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 1000

-- Properties

propPowInt :: NonNegative Integer -> NonNegative Integer -> Property
propPowInt (NonNegative n) (NonNegative i) =
  (n ^ i) === plift (pconstant n #^ pconstant i)
