{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module: Main
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Portability: GHC only
 Stability: Experimental
 Example of @plutarch-quickcheck@ tests. These are meant to be read as source
 code.
-}
module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Builtin (pforgetData)
import Plutarch.Prelude (
  PData,
  PInteger,
  POpaque,
  S,
  Term,
  pconstant,
  pdata,
  pfromData,
  pif,
  plam,
  popaque,
  ptraceInfoError,
  ptryFrom,
  tcont,
  unTermCont,
  (#),
  (#<),
  (:-->),
 )
import Plutarch.Test.QuickCheck (fromFailingPPartial, fromPPartial, pconstantT)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (
  Property,
  QuickCheckTests,
  chooseInteger,
  forAll,
  testProperty,
 )

-- This is an example pseudo validator that will pass when the sum of parameter
-- and the datum is less than 20.
-- Example doesn't use actual 'PValidator' because 'PScriptContext' is hard to
-- generate without PCB(Plutarch Context Builder).
myValidator ::
  forall (s :: S).
  Term s (PInteger :--> PData :--> PData :--> POpaque)
myValidator = plam $ \param dat _red -> unTermCont $ do
  (dat', _) <- tcont $ ptryFrom dat

  pure $
    pif
      (pfromData dat' + param #< 20)
      (popaque $ pconstant ())
      (ptraceInfoError "param + datum is larger than 20")

-- Property test. This uses 'fromPPartial'; it will fail when the validator
-- fails and will pass when validator succeed.
valProp :: Property
valProp =
  forAll (pconstantT <$> chooseInteger (0, 14)) $
    fromPPartial valScript
  where
    valScript :: forall (s :: S). Term s (PInteger :--> POpaque)
    valScript = plam $ \x ->
      myValidator
        # 5
        # pforgetData (pdata x)
        # pforgetData (pdata x)

-- When 'valProp' checks for values that should pass the validator, this
-- property checks if validator fails when given incorrect values.
-- This is somewhat of a negative case for the validator.
--
-- Unlike 'expectFailure' given by the QuickCheck, using 'fromFailingPPartial'
-- will not abort the test after getting a failure.
valPropFail :: Property
valPropFail =
  forAll (pconstantT <$> chooseInteger (15, 4000)) $
    fromFailingPPartial valScript
  where
    valScript :: forall (s :: S). Term s (PInteger :--> POpaque)
    valScript = plam $ \x ->
      myValidator
        # 5
        # pforgetData (pdata x)
        # pforgetData (pdata x)

main :: IO ()
main = do
  -- This will fix some problems regarding text encoding.
  setLocaleEncoding utf8
  defaultMain . adjustOption go $
    testGroup
      "validator tests"
      [ testProperty "Validator Property" valProp
      , testProperty "Validator Property - should fail" valPropFail
      ]
  where
    -- 100 tests is way too small for a property test to search for a counterexample,
    -- it is recommanded to use at least 10,000. However, more is better.
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
