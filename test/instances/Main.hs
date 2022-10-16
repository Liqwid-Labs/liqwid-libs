-- This ensures that our Arbitrary instances are behaving themselves.
module Main (main) where

import Data.List (nub, sort)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Test.QuickCheck.Instances ()
import PlutusLedgerApi.V2 (TokenName, Value (Value))
import qualified PlutusTx.AssocMap as AssocMap
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Property,
  conjoin,
  counterexample,
  discard,
  forAllShrinkShow,
  property,
  (.&&.),
  (===),
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain . adjustOption go . testGroup "Arbitrary instances" $
    [ testGroup
        "Value"
        [ testProperty "CurrencySymbol keys in 'outer map'" propCSKeys
        , testProperty "ADA 'inner map'" propAdaInner
        , -- This test runs slowly, so we turn down the count a bit.
          adjustOption fewer . testProperty "Other 'inner maps'" $ propOtherInner
        ]
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
    fewer :: QuickCheckTests -> QuickCheckTests
    fewer testCount = testCount `quot` 2

-- Properties

-- CurrencySymbol keys in the 'outer map' should be all of:
-- - Sorted
-- - Unique
-- - Non-empty
-- - First entry must be the ADA symbol
propCSKeys :: Property
propCSKeys = forAllShrinkShow arbitrary shrink ppShow $ \(Value val) ->
  case fst <$> AssocMap.toList val of
    [] -> failProperty "Empty 'outer map'."
    keys@(sym : _) ->
      (nub keys === keys)
        .&&. (sort keys === keys)
        .&&. (sym === "")

-- The ADA entry should be a singleton map, with the ADA token name as a key,
-- and a non-negative value
propAdaInner :: Property
propAdaInner = forAllShrinkShow arbitrary shrink ppShow $ \(Value val) ->
  case AssocMap.lookup "" val of
    Nothing -> failProperty "ADA entry not found."
    Just inner -> case AssocMap.toList inner of
      [] -> failProperty "ADA entry has empty 'inner map'."
      [(tn, amount)] -> (tn === "") .&&. property (amount >= 0)
      _ -> failProperty "ADA entry is not a singleton."

-- A non-ADA entry should:
-- - Be non-empty
-- - Have sorted, unique keys
-- - Have positive values
propOtherInner :: Property
propOtherInner = forAllShrinkShow arbitrary shrink ppShow $ \(Value val) ->
  case AssocMap.toList val of
    [] -> failProperty "Empty 'outer map'."
    [_] -> discard
    (_ : rest) ->
      let inners = AssocMap.toList . snd <$> rest
       in conjoin $ go <$> inners
  where
    go :: [(TokenName, Integer)] -> Property
    go = \case
      [] -> failProperty "Empty 'inner map'."
      inner ->
        let keys = fst <$> inner
            vals = snd <$> inner
         in (nub keys === keys)
              .&&. (sort keys === keys)
              .&&. conjoin (property . (> 0) <$> vals)

-- Helpers

failProperty :: String -> Property
failProperty msg = counterexample msg . property $ False
