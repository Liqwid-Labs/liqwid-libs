{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module NumericProp (tests) where

import Plutarch.Extra.Numeric ((#^))
import Plutarch.Test.QuickCheck (
  TestableTerm,
  fromPFun,
  pconstantT,
 )
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  QuickCheckTests,
  chooseInteger,
  counterexample,
  forAll,
  testProperty,
  (.&&.),
 )

tests :: TestTree
tests =
  adjustOption go $
    testGroup
      "Numeric"
      [ testProperty "Integer power (#^)" propPow
      ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 1

-- Properties

propPow :: Property
propPow =
  counterexample
    "x ^ 1 = x"
    ( forAll genNum $
        \x -> fromPFun powOne x
    )
    .&&. counterexample
      "x ^ 0 = 1"
      ( forAll genNum $
          \x -> fromPFun powZero x
      )
    .&&. counterexample
      "x ^ y * x ^ z = x ^ (y + z)"
      ( forAll genNum $ \x ->
          forAll genNum $ \y ->
            forAll genNum $ \z ->
              fromPFun powMult x y z
      )
    .&&. counterexample
      "x ^ (y ^ z) = x ^ (y * z)"
      ( forAll genNum $ \x ->
          forAll genNum $ \y ->
            forAll genNum $ \z ->
              fromPFun powPow x y z
      )
  where
    genNum :: Gen (TestableTerm PInteger)
    genNum = pconstantT <$> chooseInteger (0, 1000)

    powOne :: Term s (PInteger :--> PBool)
    powOne = plam $ \x -> x #== x #^ (1 :: Term s PInteger)

    powZero :: Term s (PInteger :--> PBool)
    powZero = plam $ \x -> 1 #== x #^ (0 :: Term s PInteger)

    powMult :: Term s (PInteger :--> PInteger :--> PInteger :--> PBool)
    powMult = plam $ \x y z -> (x #^ y) * (x #^ z) #== x #^ (y + z)

    powPow :: Term s (PInteger :--> PInteger :--> PInteger :--> PBool)
    powPow = plam $ \x y z -> (x #^ y) #^ z #== x #^ (y * z)
