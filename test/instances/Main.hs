-- This ensures that our Arbitrary instances are behaving themselves.
module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Test.QuickCheck.Instances ()
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)
import Value qualified

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain . adjustOption go . testGroup "Arbitrary instances" $
    [ testGroup "Value" Value.properties
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
