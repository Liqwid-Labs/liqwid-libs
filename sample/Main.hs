module Main (main) where

import GHC.IO.Encoding
import qualified MintingBuilder
import Plutarch.Context
import PlutusLedgerApi.V1
import qualified SpendingBuilder
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain . testGroup "Sample Tests" $
        [ testCase "TxInfo matches with both Minting and Spending Script Purposes" $
            (scriptContextTxInfo <$> a) @?= (scriptContextTxInfo <$> b)
        , SpendingBuilder.specs
        , MintingBuilder.specs
        ]
  where
    a = buildMinting (generalSample <> withMinting "aaaa")
    b =
        buildSpending
            ( generalSample
                <> withSpendingUTXO
                    (pubKey "aabb" . withValue (singleton "cc" "hello" 123))
            )

generalSample :: (Monoid a, Builder a) => a
generalSample =
    mconcat
        [ input $
            pubKey "aabb"
                . withValue (singleton "cc" "hello" 123)
                . withRefIndex 5
        , input $
            pubKey "eeee"
                . withValue (singleton "cc" "hello" 123)
                . withDatum (123 :: Integer)
                . withTxId "eeff"
        , output $
            script "cccc"
                . withValue (singleton "dd" "world" 123)
        , mint $ singleton "aaaa" "hello" 333
        ]
