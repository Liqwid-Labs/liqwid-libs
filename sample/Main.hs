module Main (main) where

import GHC.IO.Encoding
import Plutarch.Context
import PlutusLedgerApi.V1
import PlutusPrelude

test :: Builder a => a
test =
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

main :: IO ()
main = do
    setLocaleEncoding utf8
    let a = buildMinting test
        b = buildSpending (test <> withSpending (pubKey "aabb" . withValue (singleton "cc" "hello" 123)))
    print $ pretty $ a
    print $ pretty $ b

    print $ (scriptContextTxInfo <$> a) == (scriptContextTxInfo <$> b)
    print $ txInfoId . scriptContextTxInfo <$> a
