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
  singleton,
 )

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
    , SpendingBuilder.specs
    , MintingBuilder.specs
    ]
  where
    a = buildMinting mempty (generalSample <> withMinting "aaaa")
    b =
      buildSpending
        mempty
        ( generalSample
            <> withSpendingUTXO
              ( pubKey "aabb"
                  <> withValue (singleton "cc" "hello" 123)
                  <> withStakingCredential (StakingPtr 0 0 0)
              )
        )
    c = buildTxInfo generalSample
    d = buildTxOuts generalSample

generalSample :: (Monoid a, Builder a) => a
generalSample =
  mconcat
    [ input $
        pubKey "aabb"
          <> withValue (singleton "cc" "hello" 123)
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
