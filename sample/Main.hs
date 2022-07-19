module Main (main) where

import GHC.IO.Encoding
import Plutarch.Context
import PlutusLedgerApi.V1
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified MintingBuilder


main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain . testGroup "Sample Tests" $
      [ testCase "TxInfo matches with both Minting and Spending Script Purposes" $
          (scriptContextTxInfo <$> a) @?= (scriptContextTxInfo <$> b)
      , MintingBuilder.specs
      ]
      where
        a = buildMinting generalSample{ mbMintingCS = Just "aaaa" }
        b = buildSpending
          (generalSample <> withSpending
            (pubKey "aabb" . withValue (singleton "cc" "hello" 123)))

generalSample :: (Monoid a, Builder a) => a
generalSample  =
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
