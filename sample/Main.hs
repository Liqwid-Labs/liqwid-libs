{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (
  Config (Config, tracingMode),
  TracingMode (NoTracing),
  compile,
 )
import Plutarch.Api.V1 (PValidator)
import Plutarch.Extra.TermCont (ptryFromC)
import Plutarch.Prelude (
  PAsData,
  PInteger,
  Term,
  pdata,
  pfromData,
  pif,
  plam,
  popaque,
  ptraceError,
  unTermCont,
  (#),
  (#==),
  (:-->),
 )
import Plutarch.Test.Precompiled (
  Expectation (Failure, Success),
  testEqualityCase,
  testEvalCase,
  tryFromPTerm,
  withApplied,
  (@!>),
  (@&),
  (@>),
 )
import Test.Tasty (TestTree, defaultMain, testGroup)

import PlutusTx qualified

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "test suite"
      [ sampleValidatorTest
      , sampleFunctionTest
      ]

sampleValidator :: Term s PValidator
sampleValidator = plam $ \_ x _ -> unTermCont $ do
  (pfromData -> x', _) <- ptryFromC @(PAsData PInteger) x
  pure $
    popaque $
      pif
        (x' #== 10)
        (ptraceError "x shouldn't be 10")
        x'

sampleValidatorTest :: TestTree
sampleValidatorTest = tryFromPTerm "sample validator" sampleValidator $ do
  [PlutusTx.toData (), PlutusTx.toData (1 :: Integer), PlutusTx.toData ()] @> "It should succeed when given 1"
  [PlutusTx.toData (), PlutusTx.toData (10 :: Integer), PlutusTx.toData ()] @!> "It should fail when given 10"
  [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()] @!> "It should fail when given non integer"

  [PlutusTx.toData ()] @& do
    testEvalCase
      "(Sharing first argument) It should succeed when given 1"
      Success
      [PlutusTx.toData (1 :: Integer), PlutusTx.toData ()]

  withApplied [PlutusTx.toData ()] $ do
    testEvalCase
      "(Sharing first argument) It should fail when given 10"
      Failure
      [PlutusTx.toData (10 :: Integer), PlutusTx.toData ()]

sampleFunction :: Term s (PAsData PInteger :--> PAsData PInteger :--> PAsData PInteger)
sampleFunction = plam $ \x y -> pdata (pfromData x + pfromData y + 1)

sampleFunctionTest :: TestTree
sampleFunctionTest = tryFromPTerm "sample function" sampleFunction $ do
  [PlutusTx.toData (1 :: Integer), PlutusTx.toData (1 :: Integer)] @> "It should not fail"

  testEqualityCase "1" compiled1 [PlutusTx.toData (1 :: Integer)]
  testEqualityCase "1 1" compiled11 [PlutusTx.toData (1 :: Integer), PlutusTx.toData (1 :: Integer)]
  where
    compiled1 =
      either (error . show) id $
        compile (Config {tracingMode = NoTracing}) $
          sampleFunction # pdata 1

    compiled11 =
      either (error . show) id $
        compile (Config {tracingMode = NoTracing}) $
          sampleFunction # pdata 1 # pdata 1
