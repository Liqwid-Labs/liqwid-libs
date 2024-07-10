module Main (main) where

import Data.Bifunctor (second)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Context (
  BaseBuilder,
  Builder,
  address,
  buildMinting,
  buildMinting',
  buildSpending,
  buildTxInfo,
  buildTxOuts,
  checkNormalized,
  input,
  mint,
  mkNormalized,
  normalizeValue,
  output,
  pubKey,
  runChecker,
  script,
  withDatum,
  withMinting,
  withRefIndex,
  withRefTxId,
  withSpendingUTXO,
  withStakingCredential,
  withValue,
  withdrawal,
 )
import PlutusLedgerApi.V1 (getValue)
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass), assetClassValueOf)
import PlutusLedgerApi.V2 (
  Address (Address),
  Credential (PubKeyCredential),
  PubKeyHash (PubKeyHash),
  ScriptContext (scriptContextTxInfo),
  StakingCredential (StakingHash, StakingPtr),
  TxInfo (txInfoOutputs),
  TxOut (txOutValue),
  Value (Value),
  adaSymbol,
  adaToken,
  singleton,
  txInfoWdrl,
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
        null (runChecker checkNormalized (mkNormalized generalSample :: BaseBuilder)) @?= True
    , SpendingBuilder.specs
    , MintingBuilder.specs
    , testCase "normalizeValue removes 0 entries unless they are ADA" $
        ( getValue . normalizeValue . Value $
            AssocMap.unsafeFromList
              [ ("cc", AssocMap.unsafeFromList [("token name", 0)])
              , zeroAdaTuple
              ]
        )
          @?= (getValue . Value $ AssocMap.unsafeFromList [zeroAdaTuple])
    , testCase "normalizeValue adds 0 ADA entry if it is missing" $
        (getValue . normalizeValue . Value $ AssocMap.unsafeFromList [])
          @?= (getValue . Value $ AssocMap.unsafeFromList [zeroAdaTuple])
    , testCase "normalizeValue adds matching entires" $
        ( getValue . normalizeValue . Value $
            AssocMap.unsafeFromList
              [ zeroAdaTuple
              , ("cc", AssocMap.unsafeFromList [("token", 1)])
              , ("cc", AssocMap.unsafeFromList [("token", 1)])
              ]
        )
          @?= ( getValue . Value $
                  AssocMap.unsafeFromList
                    [ zeroAdaTuple
                    , ("cc", AssocMap.unsafeFromList [("token", 2)])
                    ]
              )
    , testCase "mkNormalized retains ADA entry in output value" $
        assetClassValueOf
          (txOutValue . head . txInfoOutputs . scriptContextTxInfo $ adaOutput10000)
          (AssetClass (adaSymbol, adaToken))
          @?= 10000
    , testCase "non-Ada tokens are dropped by `normalizeValue`" $
        ( normalizeValue . Value . AssocMap.unsafeFromList $
            [
              ( adaSymbol
              , AssocMap.unsafeFromList
                  [ (adaToken, 100)
                  , ("nonAdaToken", 100)
                  ]
              )
            ]
        )
          @?= ( Value . AssocMap.unsafeFromList $
                  [
                    ( adaSymbol
                    , AssocMap.unsafeFromList [(adaToken, 100)]
                    )
                  ]
              )
    , testCase
        "adding a withdrawal has the expected behavior when building a Txinfo"
        $ let stakingCred = StakingHash $ PubKeyCredential "abcd"
           in txInfoWdrl (buildTxInfo (withdrawal stakingCred 1))
                @?= AssocMap.unsafeFromList [(stakingCred, 1)]
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

    zeroAdaTuple = (adaSymbol, AssocMap.unsafeFromList [(adaToken, 0)])

    adaOutput10000 = buildMinting' $ mkNormalized $ output $ withValue (singleton adaSymbol adaToken 10000)

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
    AssocMap.unsafeFromList $
      second AssocMap.unsafeFromList
        <$> [ ("ccaa", [("c", 2), ("tokenhi", 10), ("hello", 30)])
            , ("ccaa", [("tokenhi", 30), ("a", 2), ("world", 40), ("b", 1)])
            , ("eeff", [("hey", 123)])
            , ("ccaa", [("hello", 20), ("b", 2), ("world", 20)])
            ]
