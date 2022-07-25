{-# LANGUAGE ViewPatterns #-}

module SpendingBuilder (specs) where

import Plutarch.Context
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import PlutusLedgerApi.V1 (ScriptContext (..), ScriptPurpose (..), TxOutRef (..))
import PlutusLedgerApi.V1.Value (singleton)

someOutRef :: TxOutRef
someOutRef = TxOutRef "abcdee" 71

sample :: SpendingBuilder
sample =
    mconcat
        [ mint $ singleton "aaaa" "hello" 333
        , input $
            pubKey "ffaacc"
                <> withValue (singleton "cc" "hello" 123)
                <> withRefIndex 19
        , input $
            pubKey "aaccdd"
                <> withValue (singleton "cc" "hello" 123)
                <> withOutRef someOutRef
        , input $
            pubKey "aabb"
                <> withValue (singleton "cc" "hello" 123)
                <> withRefIndex 1121
                <> withTxId "abababcc"
        , input $
            pubKey "eeffdd"
                <> withValue (singleton "cc" "hello" 123)
                <> withTxId "eeddaa"
        , input $
            pubKey "eeee"
                <> withValue (singleton "cc" "hello" 123)
                <> withDatum (123 :: Integer)
        , output $
            script "cccc"
                <> withValue (singleton "dd" "world" 123)
        ]

specs :: TestTree
specs =
    testGroup
        "Spending Builder Unit Tests"
        [ testCase "SpendingBuilder should fail when no input validator identifier is given" $
            case buildSpending sample of
                Left _ -> pure ()
                Right _ -> assertFailure "Builder succeed when it should have failed"
        , testCase "Set input validator identifier with TxOutRef" $
            case buildSpending (sample <> withSpendingOutRef someOutRef) of
                Left err -> assertFailure $ "Failed with error : " <> err
                Right (scriptContextPurpose -> Spending outref) -> outref @?= someOutRef
                Right _ -> assertFailure "SpendingBuilder built script context that is not spending"
        , testCase "Set input validator identifier with TxOutRefId" $
            case buildSpending (sample <> withSpendingOutRefId "abababcc") of
                Left err -> assertFailure $ "Failed with error : " <> err
                Right (scriptContextPurpose -> Spending outref) -> txOutRefId outref @?= "abababcc"
                Right _ -> assertFailure "SpendingBuilder built script context that is not spending"
        , testCase "Set input validator identifier with TxOutRefIdx" $
            case buildSpending (sample <> withSpendingOutRefIdx 19) of
                Left err -> assertFailure $ "Failed with error : " <> err
                Right (scriptContextPurpose -> Spending outref) -> txOutRefIdx outref @?= 19
                Right _ -> assertFailure "SpendingBuilder built script context that is not spending"
        , testCase "Validator identifier should be override-able" $
            case buildSpending (sample <> withSpendingOutRefIdx 19 <> withSpendingOutRef someOutRef) of
                Left err -> assertFailure $ "Failed with error : " <> err
                Right (scriptContextPurpose -> Spending outref) -> outref @?= someOutRef
                Right _ -> assertFailure "SpendingBuilder built script context that is not spending"
        , testCase "Validator identifier should be override-able 2" $
            case buildSpending
                ( sample
                    <> withSpendingOutRefIdx 19
                    <> withSpendingOutRef someOutRef
                    <> withSpendingOutRefId "abababcc"
                ) of
                Left err -> assertFailure $ "Failed with error : " <> err
                Right (scriptContextPurpose -> Spending outref) -> txOutRefId outref @?= "abababcc"
                Right _ -> assertFailure "SpendingBuilder built script context that is not spending"
        ]
