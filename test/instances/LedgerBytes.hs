module LedgerBytes (properties) where

import Helpers (failProperty, passProperty)
import Plutarch.Test.QuickCheck.Instances ()
import PlutusLedgerApi.V1.Bytes (
  LedgerBytes (LedgerBytes),
  fromHex,
 )
import PlutusLedgerApi.V2 (fromBuiltin)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Property,
  conjoin,
  counterexample,
  forAllShrinkShow,
 )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Text.Show.Pretty (ppShow)

properties :: [TestTree]
properties =
  [ testProperty "Generates hex encoding" propHexEncodedGen
  , testProperty "Shrinks hex encoding" propHexEncodedShrink
  ]

-- Properties

-- A generated LedgerByte's innards can be decoded without error
propHexEncodedGen :: Property
propHexEncodedGen =
  forAllShrinkShow arbitrary shrink ppShow $ \(LedgerBytes inner) ->
    case fromHex . fromBuiltin $ inner of
      Left err ->
        counterexample ("Encoding: " <> ppShow inner)
          . counterexample ("Error: " <> ppShow err)
          $ failProperty
      Right _ -> passProperty

-- If we shrink a generated LedgerByte, we can decode shrinks without error
propHexEncodedShrink :: Property
propHexEncodedShrink =
  forAllShrinkShow arbitrary shrink ppShow $ \lb ->
    let shrinks = shrink lb
     in conjoin $ go <$> shrinks
  where
    go :: LedgerBytes -> Property
    go lb'@(LedgerBytes inner) =
      counterexample (ppShow lb') $ case fromHex . fromBuiltin $ inner of
        Left err -> counterexample ("Error: " <> ppShow err) failProperty
        Right _ -> passProperty
