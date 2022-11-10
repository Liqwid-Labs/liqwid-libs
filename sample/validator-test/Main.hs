{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
import Plutarch.Builtin
import Plutarch.Prelude
import Plutarch.Test.QuickCheck
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.QuickCheck

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
      (ptraceError "param + datum is larger than 20")

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

main :: IO ()
main = do
  -- This will fix some problems regarding text encoding.
  setLocaleEncoding utf8
  defaultMain . adjustOption go $
    testGroup
      ""
      [ testProperty "Validator Property" valProp
      ]
  where
    -- 100 tests is way too small for a property test to search for a counterexample,
    -- it is recommanded to use at least 10,000. However, more is better.
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
