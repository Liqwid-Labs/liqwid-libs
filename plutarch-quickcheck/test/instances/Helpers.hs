module Helpers (
  failProperty,
  passProperty,
  sortedUnique,
) where

import Data.Foldable (foldl')
import Data.Kind (Type)
import Test.QuickCheck (Property, counterexample, property)
import Text.Show.Pretty (ppShow)

-- Shorthand to indicate an unconditional pass
passProperty :: Property
passProperty = property True

-- Shorthand to indicate an unconditional failure
failProperty :: Property
failProperty = property False

-- Check the argument list is both sorted and contains only unique items,
-- blowing up if it's ever not the case
sortedUnique ::
  forall (a :: Type).
  (Ord a, Show a) =>
  [a] ->
  Property
sortedUnique = \case
  [] -> passProperty
  (x : xs) -> snd . foldl' go (x, passProperty) $ xs
  where
    go :: (a, Property) -> a -> (a, Property)
    go (prev, acc) x = (x,) $ case compare prev x of
      LT -> acc
      EQ -> counterexample (explainDuplicate x) failProperty
      GT -> counterexample (explainUnordered prev x) failProperty
    explainDuplicate :: a -> String
    explainDuplicate x = "Duplicate element found: " <> ppShow x
    explainUnordered :: a -> a -> String
    explainUnordered prev curr =
      "Out of order: \n"
        <> ppShow prev
        <> "\n\nFollowed by: \n"
        <> ppShow curr
