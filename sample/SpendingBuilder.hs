{-# LANGUAGE ViewPatterns #-}

module SpendingBuilder (specs) where

import Plutarch.Context (
    SpendingBuilder,
    input,
    mint,
    output,
    pubKey,
    script,
    tryBuildSpending,
    withDatum,
    withRef,
    withRefIndex,
    withRefTxId,
    withSpendingOutRef,
    withSpendingOutRefId,
    withSpendingOutRefIdx,
    withValue,
 )
import PlutusLedgerApi.V2 (
    ScriptContext (scriptContextPurpose),
    ScriptPurpose (Spending),
    TxOutRef (..),
    singleton,
 )
import qualified Prettyprinter as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

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
                <> withRef someOutRef
        , input $
            pubKey "aabb"
                <> withValue (singleton "cc" "hello" 123)
                <> withRefIndex 1121
                <> withRefTxId "abababcc"
        , input $
            pubKey "eeffdd"
                <> withValue (singleton "cc" "hello" 123)
                <> withRefTxId "eeddaa"
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
            case tryBuildSpending mempty sample of
                Left _ -> pure ()
                Right _ -> assertFailure "Builder succeed when it should have failed"
        , testCase "Set input validator identifier with TxOutRef" $
            case tryBuildSpending mempty (sample <> withSpendingOutRef someOutRef) of
                Left err -> assertFailure $ "Failed with error : " <> show (P.pretty err)
                Right (scriptContextPurpose -> Spending outref) -> outref @?= someOutRef
                Right _ -> assertFailure "SpendingBuilder built script context that is not spending"
        , testCase "Set input validator identifier with TxOutRefId" $
            case tryBuildSpending mempty (sample <> withSpendingOutRefId "abababcc") of
                Left err -> assertFailure $ "Failed with error : " <> show (P.pretty err)
                Right (scriptContextPurpose -> Spending outref) -> txOutRefId outref @?= "abababcc"
                Right _ -> assertFailure "SpendingBuilder built script context that is not spending"
        , testCase "Set input validator identifier with TxOutRefIdx" $
            case tryBuildSpending mempty (sample <> withSpendingOutRefIdx 19) of
                Left err -> assertFailure $ "Failed with error : " <> show (P.pretty err)
                Right (scriptContextPurpose -> Spending outref) -> txOutRefIdx outref @?= 19
                Right _ -> assertFailure "SpendingBuilder built script context that is not spending"
        , testCase "Validator identifier should be override-able" $
            case tryBuildSpending mempty (sample <> withSpendingOutRefIdx 19 <> withSpendingOutRef someOutRef) of
                Left err -> assertFailure $ "Failed with error : " <> show (P.pretty err)
                Right (scriptContextPurpose -> Spending outref) -> outref @?= someOutRef
                Right _ -> assertFailure "SpendingBuilder built script context that is not spending"
        , testCase "Validator identifier should be override-able 2" $
            case tryBuildSpending
                mempty
                ( sample
                    <> withSpendingOutRefIdx 19
                    <> withSpendingOutRef someOutRef
                    <> withSpendingOutRefId "abababcc"
                ) of
                Left err -> assertFailure $ "Failed with error : " <> show (P.pretty err)
                Right (scriptContextPurpose -> Spending outref) -> txOutRefId outref @?= "abababcc"
                Right _ -> assertFailure "SpendingBuilder built script context that is not spending"
        ]
