module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Context
import PlutusLedgerApi.V2 (
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoOutputs),
    singleton,
 )
-- import qualified SpendingBuilder (specs)
-- import qualified MintingBuilder (specs)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prettyprinter (pretty)

main :: IO ()
main = do
    setLocaleEncoding utf8
    mapM_ (print . pretty) $ runChecker (checkPhase1 :: Checker () TxInfoBuilder) (testSample :: TxInfoBuilder)
    print $ buildTxInfo testSample
    defaultMain . testGroup "Sample Tests" $
        [ testCase "TxInfo matches with both Minting and Spending Script Purposes" $
            (scriptContextTxInfo <$> a) @?= (scriptContextTxInfo <$> b)
        , testCase "TxInfo from TxInfoBuilder should also match" $
            (scriptContextTxInfo <$> a) @?= Just c
        , testCase "TxOut list from TxInfoBuilder should match one from buildTxOut" $
            (txInfoOutputs . scriptContextTxInfo <$> a) @?= return d
        -- , SpendingBuilder.specs
        -- , MintingBuilder.specs
        ]
  where
    a = buildMinting (generalSample <> withMinting "aaaa")
    b =
        buildSpending
            ( generalSample
                <> withSpendingUTXO
                    (pubKey "aabb" <> withValue (singleton "cc" "hello" 123))
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
        , input $
            pubKey "eeee"
                <> withValue (singleton "cc" "hello" 123)
                <> withDatum (123 :: Integer)
                <> withTxId "eeff"
        , output $
            script "cccc"
                <> withValue (singleton "dd" "world" 123)
        , mint $ singleton "aaaa" "hello" 333
        ]

testSample :: (Monoid a, Builder a) => a
testSample = mkOutRefIndices $
    mconcat
        [ input $
            pubKey "aabb"
                <> withValue (singleton "cc" "hello" 50)
                <> withRefIndex 5
        , input $
            pubKey "eeee"
                <> withValue (singleton "cc" "hello" (negate 50) <> singleton "aa" "asdf" 1)
                <> withRefIndex 2
                <> withDatum (123 :: Integer)
        , input $
            pubKey "dddd"
                <> withValue (singleton "cc" "hello" (negate 50) <> singleton "aa" "asdf" 1)
                <> withRefIndex 5                
                <> withDatum (123 :: Integer)
        , output $
            script "cccc"
                <> withValue (singleton "cc" "hello" 100 <> singleton "aaaa" "hello" 333)
        , mint $ singleton "aaaa" "hello" 333
        , mint $ singleton "" "" 123
        , fee $ singleton "aa" "zxcv" 1203
        ]        
