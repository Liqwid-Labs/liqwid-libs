{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Properties.TraversableProp (tests) where

import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.Extra.Traversable (PTraversable, ptraverse)
import Plutarch.Extra.Functor (PSubcategory)
import Plutarch.Extra.Identity (PIdentity(PIdentity))
import Plutarch.Test.QuickCheck (PA, PArbitrary, punlam)
import Test.QuickCheck (
  Property,
  counterexample,
  forAllShrinkShow,
  arbitrary,
  shrink,
 )
import Test.Tasty (adjustOption, testGroup, TestTree)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

tests :: TestTree
tests =
  adjustOption go $ testGroup "Traversable" $
  [ testProperty "PBuiltinList is Traversable" $ (traversableLaw @PList)
  , testProperty "PMaybe is Traversable" $ (traversableLaw @PMaybe)
  , testProperty "PMaybeData is Traversable" $ (traversableLaw @(PMaybeData))
  , testProperty "PEither is Traversable" $ (traversableLaw @(PEither PUnit))
  , testProperty "PList is Traversable" $ (traversableLaw @(PList))
  , testProperty "PPair is Traversable" $ (traversableLaw @(PPair PA))
  ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 1000

-- Properties

traversableLaw ::
    forall (t :: (S -> Type) -> S -> Type).
    ( PTraversable t
    , PEq (t PA)
    , PArbitrary (t PA)
    , PSubcategory t PA
    ) =>
    Property
traversableLaw =
  counterexample "identity" $
    forAllShrinkShow arbitrary shrink (const "") (punlam @PBool identity)
    -- Note from Seungheon
    -- Here, I intentionally used 'punlam' instead of conventional 'fromPFun'
    -- because 'fromPFun' checks for lambda(function generation) which will
    -- break down when given ambiguous type variable like 't' in this case.
    -- Since we know that 't' is NOT a function, we can use 'punlam' and
    -- prevent the error.
    --
    -- Alternatively, we can ensure that 't' is not a lambda by giving
    -- `IsLam ((t PA) :--> PBool) ~ False`.
  where
    identity :: Term s (t PA :--> PBool)
    identity = plam $ \x ->
        ptraverse @t # (plam $ \y -> pcon $ PIdentity y) # x
        #== (pcon $ PIdentity x)
