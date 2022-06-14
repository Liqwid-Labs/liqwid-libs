{-# LANGUAGE PartialTypeSignatures #-}

module Spec.Api.V1.Value.Unsorted (tests) where

--------------------------------------------------------------------------------

import Spec.Utils (
    genValue,
    sortValue,
 )
import Test.QuickCheck (
    Property,
    shrinkNothing,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Plutarch.Property (peqPropertyNative')
import Test.Tasty.QuickCheck (
    testProperty,
 )

--------------------------------------------------------------------------------

import Plutarch (phoistAcyclic, plam, (#))
import Plutarch.Unsafe (punsafeCoerce)
import qualified PlutusLedgerApi.V1.Value as Value

--------------------------------------------------------------------------------

import Plutarch.Api.V1.Value.Unsorted (psort)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testProperty "'psort' sorts unsorted values as expcted" prop_psortCorrect
    ]

-- | The property of 'psort' to make an unsorted value sorted.
prop_psortCorrect :: Property
prop_psortCorrect =
    peqPropertyNative'
        expected
        genValue
        shrinkNothing
        definition
  where
    expected = Value.getValue . sortValue
    definition = phoistAcyclic $
        plam $ \m ->
            punsafeCoerce $ psort # m
