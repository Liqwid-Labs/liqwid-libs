module Value (properties) where

import Helpers (failProperty, sortedUnique)
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
import Test.Tasty (TestTree, adjustOption)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Text.Show.Pretty (ppShow)

properties :: [TestTree]
properties =
  [ testProperty "CurrencySymbol keys in 'outer map'" propCSKeys
  , testProperty "ADA 'inner map'" propAdaInner
  , -- This test runs slowly, so we turn down the count a bit
    adjustOption fewer . testProperty "Other 'inner maps'" $ propOtherInner
  ]
  where
    fewer :: QuickCheckTests -> QuickCheckTests
    fewer testCount = testCount `quot` 2

-- CurrencySymbol keys in the 'outer map' should be all of:
-- - Sorted and unique
-- - Non-empty
-- - First entry must be the ADA symbol
propCSKeys :: Property
propCSKeys = forAllShrinkShow arbitrary shrink ppShow $ \(Value val) ->
  case fst <$> AssocMap.toList val of
    [] -> counterexample "Empty 'outer map'." failProperty
    keys@(sym : _) -> sortedUnique keys .&&. (sym === "")

-- The ADA entry should be a singleton map, with the ADA token name as a key,
-- and a non-negative value
propAdaInner :: Property
propAdaInner = forAllShrinkShow arbitrary shrink ppShow $ \(Value val) ->
  case AssocMap.lookup "" val of
    Nothing -> counterexample "ADA entry not found." failProperty
    Just inner -> case AssocMap.toList inner of
      [] -> counterexample "ADA entry has empty 'inner map'." failProperty
      [(tn, amount)] -> (tn === "") .&&. property (amount >= 0)
      _ -> counterexample "ADA entry is not a singleton." failProperty

-- A non-ADA entry should:
-- - Be non-empty
-- - Have sorted, unique keys
-- - Have positive values
propOtherInner :: Property
propOtherInner = forAllShrinkShow arbitrary shrink ppShow $ \(Value val) ->
  case AssocMap.toList val of
    [] -> counterexample "Empty 'outer map'." failProperty
    [_] -> discard
    (_ : rest) ->
      let inners = AssocMap.toList . snd <$> rest
       in conjoin $ go <$> inners
  where
    go :: [(TokenName, Integer)] -> Property
    go = \case
      [] -> counterexample "Empty 'inner map'." failProperty
      inner ->
        let keys = fst <$> inner
            vals = snd <$> inner
         in sortedUnique keys .&&. conjoin (property . (> 0) <$> vals)
