module Main(main) where

import GHC.IO.Encoding
import Plutarch.Context
import PlutusLedgerApi.V1
import PlutusPrelude

test :: Builder a => a
test =
  mconcat
  [ input
    $ pubKey "aabb"
    . withValue (singleton "cc" "hello" 123)
    . withRefIndex 5
  , input
    $ pubKey "eeee"
    . withValue (singleton "cc" "hello" 123)
    . withDatum (123 :: Integer)
  , output
    $ script "cccc"
    . withValue (singleton "dd" "world" 123)
  ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  print $ pretty $ spends <$> buildTxInfo defaultConfig test
  print $ pretty $ buildSpending defaultConfig (test <> fromValidator (pubKey "aabb" . withValue (singleton "cc" "hello" 123)))
