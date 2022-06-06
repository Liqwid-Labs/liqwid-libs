module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Minting (mintingBuilderProperty)
import Spending (spendingBuilderProperty)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain . adjustOption go $
        testGroup
            "context builder"
            [ testProperty "spending" spendingBuilderProperty
            , testProperty "minting" mintingBuilderProperty
            ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
