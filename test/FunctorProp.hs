{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}

module FunctorProp (tests) where

import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.Extra.Functor
import Plutarch.Test.QuickCheck (
  FromPFun,
  PA,
  PArbitrary,
  PLamWrapped,
  TestableTerm,
  fromPFun,
 )
import Test.QuickCheck (
  Property,
  arbitrary,
  counterexample,
  forAllShrink,
  forAllShrinkShow,
  resize,
  shrink,
  (.&&.),
 )
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

tests :: TestTree
tests =
  adjustOption go $
    testGroup
      "Functor"
      [ testProperty "PBuiltinList is Functor" $ functorLaw @PList
      , testProperty "PMaybe is Functor" $ functorLaw @PMaybe
      , testProperty "PMaybeData is Functor" $ functorLaw @PMaybeData
      , testProperty "PEither is Functor" $ functorLaw @(PEither PUnit)
      , testProperty "PList is Functor" $ functorLaw @PList
      , testProperty "PPair is Functor" $ functorLaw @(PPair PA)
      ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 1000

-- Properties

functorLaw ::
  forall (f :: (S -> Type) -> S -> Type).
  ( PFunctor f
  , PEq (f PA)
  , PSubcategory f PA
  , PArbitrary (f PA)
  , FromPFun PBool (f PA :--> PBool)
  , PLamWrapped (TestableTerm (f PA) -> TestableTerm PBool)
      ~ (TestableTerm (f PA) -> TestableTerm PBool) -- TODO This can be better
  ) =>
  Property
functorLaw =
  counterexample
    "Composition Law"
    ( forAllShrink (resize 10 arbitrary) shrink $ \x ->
        forAllShrink (resize 10 arbitrary) shrink $ \y ->
          forAllShrinkShow
            (resize 20 arbitrary)
            shrink
            (const "")
            $ \z ->
              fromPFun composition x y z
    )
    .&&. counterexample "Identity Law" (forAllShrinkShow arbitrary shrink (const "") (fromPFun identity))
  where
    composition ::
      Term
        s
        ( (PA :--> PA)
            :--> (PA :--> PA)
            :--> f PA
            :--> PBool
        )
    composition = plam $ \f g x ->
      (pfmap # plam (\y -> g #$ f # y) # x) #== (pfmap # g # (pfmap # f # x))

    identity :: Term s (f PA :--> PBool)
    identity = plam $ \x ->
      (pfmap # plam id # x) #== x
