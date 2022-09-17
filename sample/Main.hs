{-# LANGUAGE TypeApplications #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Context
import PlutusLedgerApi.V2 (
  Address (Address),
  Credential (PubKeyCredential),
  PubKeyHash (PubKeyHash),
  ScriptContext (scriptContextTxInfo),
  StakingCredential (StakingPtr),
  TxInfo (txInfoOutputs),
  Value (Value),
  singleton,
 )
import PlutusTx.AssocMap qualified as AssocMap

import MintingBuilder qualified (specs)
import SpendingBuilder qualified (specs)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain . testGroup "Sample Tests" $
    [ testCase "TxInfo matches with both Minting and Spending Script Purposes" $
        scriptContextTxInfo a @?= scriptContextTxInfo b
    , testCase "TxInfo from TxInfoBuilder should also match" $
        scriptContextTxInfo a @?= c
    , testCase "TxOut list from TxInfoBuilder should match one from buildTxOut" $
        txInfoOutputs (scriptContextTxInfo a) @?= d
    , testCase "Checker should fail when there is a value that is not normalized" $
        null (runChecker checkNormalized (generalSample :: BaseBuilder)) @?= False
    , testCase "Checker should succeed when there is a value that is not normalized" $
        null (runChecker checkNormalized (mkNormalized $ generalSample :: BaseBuilder)) @?= True
    , SpendingBuilder.specs
    , MintingBuilder.specs
    ]
  where
    a = buildMinting mempty (mkNormalized $ generalSample <> withMinting "aaaa")
    b =
      buildSpending
        mempty
        ( mkNormalized $
            generalSample
              <> withSpendingUTXO
                ( pubKey "aabb"
                    <> withValue nonNormalizedValue
                    <> withRefIndex 5
                    <> withStakingCredential (StakingPtr 0 0 0)
                )
        )
    c = buildTxInfo $ mkNormalized generalSample
    d = buildTxOuts $ mkNormalized generalSample

generalSample :: (Monoid a, Builder a) => a
generalSample =
  mconcat
    [ input $
        pubKey "aabb"
          <> withValue nonNormalizedValue
          <> withRefIndex 5
          <> withStakingCredential (StakingPtr 0 0 0)
    , input $
        address (Address (PubKeyCredential $ PubKeyHash "aa") (Just $ StakingPtr 1 2 3))
          <> withValue (singleton "cc" "hello" 123)
          <> withDatum (123 :: Integer)
          <> withRefTxId "eeff"
    , output $
        script "cccc"
          <> withValue (singleton "dd" "world" 123)
    , mint $ singleton "aaaa" "hello" 333
    ]

nonNormalizedValue :: Value
nonNormalizedValue =
  Value $
    AssocMap.fromList $
      (\(x, y) -> (x, AssocMap.fromList y))
        <$> [ ("ccaa", [("c", 2), ("tokenhi", 10), ("hello", 30)])
            , ("ccaa", [("tokenhi", 30), ("a", 2), ("world", 40), ("b", 1)])
            , ("eeff", [("hey", 123)])
            , ("ccaa", [("hello", 20), ("b", 2), ("world", 20)])
            ]
