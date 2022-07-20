-- | Helpers for making exhaustive generators.
module Test.Benchmark.Exhaustive (exhGenBags) where

{- | Exhaustively generate lists (bags) of a given size.

 No list is generated twice.
 Duplicate elements are allowed.
 Output list elements are ordered like in the input list of unique elements.
 Thus, duplicate elements are always grouped in a contiguous sublist.
-}
exhGenBags ::
  -- | number of elems to be generated
  Int ->
  -- | choice of unique elements (a set)
  [a] ->
  [[a]]
-- TODO should use dlist
exhGenBags num [x] = pure $ replicate num x
exhGenBags 0 _ = mempty
exhGenBags num (x : xs) = do
  reps <- [0 .. num]
  (replicate reps x <>) <$> exhGenBags (num - reps) xs
exhGenBags _ [] = error "no choice"