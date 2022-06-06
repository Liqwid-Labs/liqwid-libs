{-# LANGUAGE ViewPatterns #-}

module Spending(spendingBuilderProperty) where

--import Plutarch.Context
import PlutusLedgerApi.V1
import Test.Tasty.QuickCheck
import Plutarch.Context
import Data.Maybe

import BaseBuilder

data ValidatorInput
  = NoValidatorInput
  | InputFromValidator ValidatorHash Value
  | InputFromValidatorWith ValidatorHash Value Integer
  deriving stock (Show)

data SpendingBuilderInterface
  = SBI [BuilderInterface] ValidatorInput
  deriving stock (Show)

data ExpectedResult
    = ShouldBuild
    | Shouldn'tBuild
    deriving stock (Show, Eq)

toSpendingBuilder :: SpendingBuilderInterface -> SpendingBuilder
toSpendingBuilder (SBI infs NoValidatorInput)
  = build infs
toSpendingBuilder (SBI infs (InputFromValidator vhash val))
  = build infs <> inputFromValidator vhash val
toSpendingBuilder (SBI infs (InputFromValidatorWith vhash val dat))
  = build infs <> inputFromValidatorWith vhash val dat

genCases :: Gen (SpendingBuilderInterface, ExpectedResult)
genCases = do
    infs <- arbitrary

    vin <- do
      valhash <- ValidatorHash . toBuiltin <$> genHashByteString
      val <- genAnyValue
      dat <- (arbitrary :: Gen Integer)
      elements
        [ NoValidatorInput
        , NoValidatorInput -- Balance tags
        , InputFromValidator valhash val
        , InputFromValidatorWith valhash val dat
        ]

    let expectation = case vin of
                        NoValidatorInput -> Shouldn'tBuild
                        _ -> ShouldBuild
    return $ (SBI infs vin, expectation)

shrinkCases ::
  (SpendingBuilderInterface, ExpectedResult) ->
  [(SpendingBuilderInterface, ExpectedResult)]
shrinkCases (SBI infs vin, e) = (, e) . (flip SBI vin) <$> shrink infs

spendingRules :: ScriptContext -> ValidatorInput -> Property
spendingRules context vin = go vin .&&. purpose
  where
    ins = txInfoInputs . scriptContextTxInfo $ context
    vhashToAddr = (flip Address Nothing) . ScriptCredential
    datumPairs = txInfoData . scriptContextTxInfo $ context

    purpose = case scriptContextPurpose context of
                Spending outref -> property $ any (\x -> txInInfoOutRef x == outref) ins
                _ -> property $ False
    
    go NoValidatorInput = property $ False
    go (InputFromValidator (vhashToAddr -> addr) val) = property $ 
      check (addr, val) (txInInfoResolved <$> ins)
    go (InputFromValidatorWith (vhashToAddr -> addr) val dat) = property $
      checkWithDatum (addr, val, dat) (txInInfoResolved <$> ins)
      && datumExists datumPairs dat

spendingBuilderProperty :: Property
spendingBuilderProperty = forAllShrink genCases shrinkCases go
  where
    go (sbi@(SBI infs vin), tag) = checkCoverage $
      cover 50.0 (tag == Shouldn'tBuild) "shouldn't pass" $
      cover 50.0 (tag == ShouldBuild) "should pass" $
      case tag of
        ShouldBuild ->
          case ctx of
            Just sc -> baseRules sc infs .&&. spendingRules sc vin
            Nothing -> property False
        Shouldn'tBuild -> property $ isNothing ctx
            
      where
        ctx = buildSpending defaultConfig $ toSpendingBuilder sbi
      
