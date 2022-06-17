module Spec.Api.V1.Value (tests) where

import qualified Spec.Api.V1.Value.Unsorted as Unsorted
import Test.Tasty (TestTree, testGroup)

tests :: [TestTree]
tests =
    [ testGroup "unsorted values" Unsorted.tests
    ]
